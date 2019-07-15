from colorlog import info

from yangson.instance import InstanceRoute
from yangson.exceptions import NonexistentInstance

from jetconf.helpers import JsonNodeT, PathFormat
from jetconf.handler_list import OP_HANDLERS
from jetconf.data import BaseDatastore


# ---------- User-defined handlers follow ----------

class OpHandlersContainer:
    def __init__(self, ds: BaseDatastore):
        self.ds = ds

    def jukebox_play_op(self, input_args: JsonNodeT, username: str) -> JsonNodeT:
        # Structure of RPC's input and output arguments is defined in YANG data model
        # Play song
        req_pl_name = input_args["example-jukebox:playlist"]
        req_song_index = input_args["example-jukebox:song-number"]

        info("Called operation 'jukebox_play_op' by user '{}':".format(username))

        root = self.ds.get_data_root()

        # Fetch playlists from datastore
        playlists_ii = self.ds.parse_ii("/example-jukebox:jukebox/playlist", PathFormat.URL)
        try:
            playlists = root.goto(playlists_ii).value
        except NonexistentInstance:
            raise ValueError("No playlists present in datastore")

        # Find playlist with desired name
        pls_found = list(filter(lambda p: p["name"] == req_pl_name, playlists))
        if pls_found:
            pl = pls_found[0]
        else:
            raise ValueError("Cannot find playlist \"{}\"".format(req_pl_name))

        # Find song in playlist
        pl_songs = pl["song"]

        try:
            song = pl_songs[req_song_index]
        except IndexError:
            raise ValueError("Selected playlist does not contain that much ({}) songs".format(req_song_index + 1))

        # Get reference to song record in library
        song_id = song["id"]    # type: InstanceRoute
        try:
            lib_song = root.goto(song_id).value
        except NonexistentInstance:
            # This should never happen with validated datastore,
            # data model ensures that songs in playlist are also present in the library
            raise ValueError("Requested song was found in playlist, but not in library")

        # Get file location
        lib_song_location = lib_song["location"]

        info("Playlist name: {}".format(req_pl_name))
        info("Song number: {}".format(req_song_index))
        info("File location: {}".format(lib_song_location))


def register_op_handlers(ds: BaseDatastore):
    op_handlers_obj = OpHandlersContainer(ds)
    OP_HANDLERS.register(op_handlers_obj.jukebox_play_op, "example-jukebox:play")
