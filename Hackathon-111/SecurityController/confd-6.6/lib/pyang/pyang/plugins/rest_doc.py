"""NCS/ConfD REST documentation generator
"""

import optparse
import sys
import re
import string
"""REST documentation generator"""

import types
import StringIO

from pyang import plugin
from pyang import statements
from pyang.statements import mk_path_str


def pyang_plugin_init():
    plugin.register_plugin(RestDocPlugin())

class RestDocPlugin(plugin.PyangPlugin):

    def add_output_format(self, fmts):
        self.multiple_modules = True
        fmts['rest-doc'] = self

    def add_opts(self, optparser):
        optlist = [
            optparse.make_option('--rest-doc-db',
                                 dest = 'db',
                                 choices = ['candidate', 'running'],
                                 default = 'running',
                                 help = 'Database to use'),

            optparse.make_option('--rest-doc-address',
                                 dest = 'restAddress',
                                 default = 'http://localhost:8080',
                                 help = 'REST address'),

            optparse.make_option('--rest-doc-output',
                                 dest = 'outputType',
                                 choices = ['get', 'get_curl',
                                            'patch', 'patch_curl',
                                            'put', 'put_curl',
                                            'post', 'post_curl',
                                            'delete', 'delete_curl',
                                            'docbook'],
                                 default = 'get',
                                 help = 'Output format'),

            optparse.make_option('--rest-doc-query-type',
                                 dest = 'queryType',
                                 choices = ['default', 'shallow', 'deep'],
                                 default = 'default',
                                 help = 'Output format'),


            optparse.make_option('--rest-doc-end-list-url-with-keys',
                                 dest = 'endListUrlWithKeys',
                                 action = 'store_true',
                                 default = True,
                                 help = 'List URLs end with keys'),

            optparse.make_option('--rest-doc-end-list-url-with-no-keys',
                                 dest = 'endListUrlWithKeys',
                                 action = 'store_false',
                                 help = 'List URLs don\'t end with keys'),


            optparse.make_option('--rest-doc-list-keys',
                                 dest = 'listKeys',
                                 default = None,
                                 help = 'Dictionary of list keys and values'),

            optparse.make_option('--rest-doc-path',
                                 dest = 'stmtPath',
                                 default = None,
                                 help = 'Generate output for specified path'),

            optparse.make_option('--rest-doc-curl-flags',
                                 dest = 'curlFlags',
                                 default = '',
                                 help = 'Sub-statements depth'),

            optparse.make_option('--rest-doc-show-description',
                                 dest = 'showDescription',
                                 action = 'store_true',
                                 default = False,
                                 help = 'Show description and tailf:info content')

            ]

        g = optparser.add_option_group("REST documentation specific options")
        g.add_options(optlist)


    def setup_ctx(self, ctx):
        ctx.opts.restUser = 'admin:admin'
        ctx.opts.restAcceptValue = '<default>'
        ctx.opts.restAccept = None
        ctx.opts.restContentType = None

        ctx.opts.stmts = None

        # Only used by docbook generator
        ctx.opts.curlNewLine = False

        if ctx.opts.listKeys != None :
            k = ctx.opts.listKeys
            k = eval(k)

            if type(k) != types.DictType :
                raise Exception('listKeys must be a valid dictionary, was a %s' % (type(k)))

            ctx.opts.listKeys = k

        ctx.opts.iterLevel = 1


    def setup_fmt(self, ctx):
        ctx.implicit_errors = False

    def emit(self, ctx, modules, fd):
        msh = ModulesHelper(modules)

        if ctx.opts.stmtPath :
            ctx.opts.stmts = msh.path_to_stmts(ctx.opts.stmtPath)
            if not ctx.opts.stmts :
                raise Exception('%s corresponds to no statement' % (
                                    ctx.opts.stmtPath))

        ae = RestDocEmitter()
        ae.emit(ctx, modules, fd)


# ------------------------------------------------------------------------------

class ModulesHelper(object) :

    def __init__(self, modules) :
        self.modules = modules

    # FIXME : Better algorithm
    def path_to_stmts(self, path) :
        stmts = []

        for module in self.modules :
            stmt = self.__path_to_stmt(path, module)

            # FIXME : Only handle multiple statements from different modules
            if stmt :
                stmts.append(stmt)

        if stmts :
            return stmts


    def __path_to_stmt(self, path, stmt) :

        if type(stmt.keyword) == types.TupleType :
            # Unknown statement
            return

        if stmt.parent :
            p = mk_path_str(stmt, False)

            if p != path :
                p = mk_path_str(stmt, True)

            if p == path :
                if not stmt.keyword == 'key' :
                    return stmt

        if stmt.substmts :
            for s in stmt.substmts :
                ret = self.__path_to_stmt(path, s)

                if ret :
                    return ret


# ------------------------------------------------------------------------------

class ModuleHelper(object) :

    def __init__(self, module) :
        self.module = module

    def get_prefix(self) :
        return self.module.search_one('prefix').arg



# ------------------------------------------------------------------------------

class NcsServiceHelper(object) :

    def __init__(self) :
        pass

    def stmt_contains_service(self, stmt) :
        for s in stmt.substmts :
            if (s.keyword == 'uses') and (s.arg == 'ncs:service-data') :
                return True

        return False

    def is_service_stmt(self, stmt) :
        if (stmt.keyword == 'container') and (stmt.arg == 'private') :
            for s in stmt.substmts :
                if (s.keyword == ('tailf-common', 'hidden')) and (s.arg == 'fastmap-private') :
                    return True

        return False

# ------------------------------------------------------------------------------

class StmtHelper(object) :

    def __init__(self) :
        pass


    def get_plain_path(self, stmt) :
        return self.__get_plain_path(stmt, '')

    def __get_plain_path(self, stmt, acc) :
        kw = stmt.keyword

        if (kw == 'module') or (kw == 'submodule') :
            return acc

        return self.__get_plain_path(stmt.parent, '/' + stmt.arg + acc)

    def is_unknown_stmt(self, stmt) :
        return type(stmt.keyword) == types.TupleType

# ------------------------------------------------------------------------------

class RestHelper(object) :

    def __init__(self) :
        pass

    def stmt_path(self, stmt, withPrefix = False) :
        return mk_path_str(stmt, withPrefix)

    def get_type_stmt(self, stmt) :
        ts = stmt.search_one('type')
        return ts

    def is_oper_stmt(self, stmt) :
        cf = stmt.search_one('config')

        if cf and (cf.arg == 'false') :
            return True
        else :
            return False

    def rest_api_path(self, db, stmt, query = None,
                      lastListKeys = False, urlEncode = False,
                      listKeys = None, pathWithPrefix = False,
                      dropLastListInPath = False) :

        if dropLastListInPath and (stmt.keyword == 'list') :
            stmt = stmt.parent

        ret = '/api/' + db + self.__rest_path(stmt, '', lastListKeys,
                                              urlEncode, listKeys,
                                              pathWithPrefix)

        if query and (query != 'default') :
            ret = ret + '?' + query

        return ret

    def __rest_path(self, stmt, acc, lastListKeys, urlEncode,
                    listKeys, pathWithPrefix) :
        kw = stmt.keyword

        if (kw == 'module') or (kw == 'submodule') :
            return acc
        elif kw == 'list' :
            if acc == '' :
                if lastListKeys :
                    acc = '/' + self.__get_list_keys(stmt, urlEncode, listKeys)
            else :
                acc = '/' + self.__get_list_keys(stmt, urlEncode, listKeys) + acc

        arg = stmt.arg
        if pathWithPrefix :
            prefix = stmt.i_module.i_prefix
            arg = prefix + ':' + arg

        if kw in ['choice', 'case'] :

            # Ignore these statments in the path
            return self.__rest_path(stmt.parent, acc,
                                    lastListKeys, urlEncode, listKeys,
                                    pathWithPrefix)
        else :
            return self.__rest_path(stmt.parent, '/' + arg + acc,
                                    lastListKeys, urlEncode, listKeys,
                                    pathWithPrefix)


    def get_list_keys(self, listStmt) :
        assert listStmt.keyword == 'list'

        keyStmt = listStmt.search_one('key')
        items = keyStmt.arg.strip().split()

        return items

    def get_list_keys_count(self, listStmt) :
        return len(self.get_list_keys(listStmt))

    def get_key_name(self, keyName, listKeys = None) :
        item = keyName.strip()

        if listKeys and (item in listKeys) :
            return listKeys[item]
        else :
            return '<<' + item.upper() + '>>'

    def __get_list_keys(self, listStmt, urlEncode, listKeys) :
        ret = ''

        items = self.get_list_keys(listStmt)

        if items :
            for item in items :
                if ret :
                    ret += ','

                key = str(self.get_key_name(item, listKeys = listKeys))
                key = key.replace(',', '%2C')

                ret += key
        else :
            ret += '<<<NO KEY>>>'

        return ret

    def get_one_line_stmt_info(self, stmt) :
        path = self.stmt_path(stmt, withPrefix = True)
        kw = stmt.keyword

        if kw == 'leaf' :
            ts = self.get_type_stmt(stmt)

            if ts :
                kw += ' (' + ts.arg + ')'

        return '%s %s' % (kw, path)

    def get_stmt_description(self, stmt) :
        ret = None

        for ss in stmt.substmts :
            if ss.keyword == 'description' :
                ret = ss.arg
                break
            elif ss.keyword == ('tailf-common', 'info') :
                ret = ss.arg
                break

        return ret

    def line_split_stmt_description(self, descr) :
        ret = descr.split('\n')
        return ret

# ------------------------------------------------------------------------------

class RestSimpleVerbHelper(object) :

    def __init__(self) :
        self.rh = RestHelper()

    def _emit_begin(self, f, stmt) :
        li = self.rh.get_one_line_stmt_info(stmt)
        f.write('# %s\n' % (li))

    def _emit_end(self, f) :
        f.write('\n')

    def _emit_description(self, f, stmt) :
        descr = self.rh.get_stmt_description(stmt)
        if descr :
            lines = self.rh.line_split_stmt_description(descr)
            f.write('#\n')
            f.write('# Description:\n')

            for line in lines :
                f.write('#     %s\n' % (line.strip()))

            f.write('#\n')

    def _emit_headers(self, f, isCollection = False) :
        if isCollection :
            f.write('Accept: application/vnd.yang.collection+json\n')
        else :
            f.write('Accept: application/vnd.yang.data+json\n')

    def _is_body_stmt(self, stmt, additionalStmtKeywords = None) :
        kw = stmt.keyword

        if type(kw) == types.TupleType :
            # Unknown statement
            return False


        if additionalStmtKeywords and (kw in additionalStmtKeywords) :
            return True

        return stmt.keyword in ['container', 'list', 'leaf', 'choice', 'case']

    def _get_expanded_body_stmts(self, stmts, additionalStmtKeywords = None) :
        ret = []

        for s in stmts :
            if self.rh.is_oper_stmt(s) :
                # No oper stmts in the PPP case
                pass
            elif self._is_body_stmt(s, additionalStmtKeywords) :
                ret.append(s)
            elif s.keyword == 'uses' :
                ret.extend(self._get_expanded_body_stmts(s.i_grouping.substmts))

        return ret

    def _get_expanded_body_stmts_count(self, stmts) :
        return len(self._get_expanded_body_stmts(stmts))

class RestGetHelper(RestSimpleVerbHelper) :

    def __init__(self) :
        RestSimpleVerbHelper.__init__(self)

    def emit_get_begin(self, f, stmt) :
        self._emit_begin(f, stmt)

    def emit_get_end(self, f) :
        self._emit_end(f)

    def emit_get_description(self, f, stmt) :
        self._emit_description(f, stmt)

    def emit_get_headers(self, f, isCollection = False) :
        self._emit_headers(f, isCollection)

    def emit_get(self, f, db, stmt,
                 query = None, lastListKeys = False,
                 urlEncode = False, pathWithPrefix = False) :

        path = self.rh.rest_api_path(db, stmt, query = query,
                                     lastListKeys = lastListKeys,
                                     urlEncode = urlEncode,
                                     pathWithPrefix = pathWithPrefix)
        f.write('GET %s\n' % (path))


class RestDeleteHelper(RestSimpleVerbHelper) :

    def __init__(self) :
        RestSimpleVerbHelper.__init__(self)

    def emit_delete_begin(self, f, stmt) :
        self._emit_begin(f, stmt)

    def emit_delete_end(self, f) :
        self._emit_end(f)

    def emit_delete_description(self, f, stmt) :
        self._emit_description(f, stmt)

    def emit_delete_headers(self, f) :
        self._emit_headers(f)

    def emit_delete(self, f, db, stmt,
                    lastListKeys = False, urlEncode = False,
                    pathWithPrefix = False) :
        path = self.rh.rest_api_path(db, stmt,
                                     lastListKeys = lastListKeys,
                                     urlEncode = urlEncode,
                                     pathWithPrefix = pathWithPrefix)
        f.write('DELETE %s\n' % (path))


# ------------------------------------------------------------------------------

# Patch Post Put : PPP
class RestPPPHelper(RestSimpleVerbHelper) :

    def __init__(self) :
        RestSimpleVerbHelper.__init__(self)

    def emit_ppp_begin(self, verb, f, stmt) :
        li = self.rh.get_one_line_stmt_info(stmt)
        f.write('# %s\n' % (li))

    def emit_ppp_end(self, verb, f) :
        f.write('\n')

    def emit_ppp_description(self, f, stmt) :
        self._emit_description(f, stmt)

    def emit_ppp_url(self, verb, f, db, stmt, lastListKeys = False,
                     urlEncode = False, pathWithPrefix = False,
                     dropLastListInPath = False) :

        if dropLastListInPath and (stmt.keyword == 'list') :
            stmt = stmt.parent

        path = self.rh.rest_api_path(db, stmt,
                                     lastListKeys = lastListKeys,
                                     urlEncode = urlEncode,
                                     pathWithPrefix = pathWithPrefix)
        f.write('%s %s\n' % (verb, path))

    def emit_ppp_headers(self, f) :
        f.write('Content-Type: application/vnd.yang.data+json\n')

    def emit_ppp_body(self, f, stmt) :
        f.write('{\n')

        # We are the top node
        peerCount = 1
        self.__emit_stmt(f, stmt, 1, peerCount)

        f.write('}\n')


    def emit_ppp(self, verb, f, db, stmt,
                   lastListKeys = False, urlEncode = False,
                   pathWithPrefix = False) :

        self.emit_ppp_url(verb, f, db, stmt, lastListKeys, urlEncode,
                            pathWithPrefix = pathWithPrefix)

        self.emit_ppp_headers(f)
        self.emit_ppp_body(f, stmt)

    def emit_ppp_curl(self, verb, f, address, contentType, user, db, stmt,
                      listKeys = None, newLine = False,
                      pathWithPrefix = False, curlFlags = None,
                      dropLastListInPath = False) :

        f.write('echo \'\n')

        f.write('{\n')

        # We are the top node
        peerCount = 1
        self.__emit_stmt(f, stmt, 1, peerCount, listKeys)

        f.write('}\n')

        f.write('\' | ')

        if newLine :
            f.write('\\\n')

        rch = RestCurlHelper()
        cs = rch.get_curl_ppp_str(verb, address, contentType, user, db, stmt,
                                  dataFromStdin = True,
                                  newLine = newLine,
                                  pathWithPrefix = pathWithPrefix,
                                  curlFlags = curlFlags,
                                  dropLastListInPath = dropLastListInPath)

        f.write('%s\n' % (cs))


    def __get_prefix(self, level) :
        return ''.rjust(2 * level)

    def __emit_stmt(self, f, stmt, level, peerAcc, listKeys = None) :
        prefix = self.__get_prefix(level)

        kw = stmt.keyword

        if type(kw) == types.TupleType :
            # Unknown statement
            return

        if kw == 'container' :
            self.__emit_patch_container(f, prefix, stmt, level,
                                        peerAcc, listKeys)
        elif kw == 'leaf' :
            self.__emit_patch_leaf(f, prefix, stmt, peerAcc, listKeys)
        elif kw == 'list' :
            self.__emit_patch_list(f, prefix, stmt, level, listKeys)
        elif kw == 'choice' :
            self.__emit_patch_choice(f, stmt, level, peerAcc, listKeys)
        elif kw == 'case' :
            self.__emit_patch_case(f, stmt, level, peerAcc, listKeys)
        elif kw == 'description' :
            # Nothing to do here
            pass
        else :
            pass


    def __emit_patch_container(self, f, prefix, stmt, level,
                               peerAcc, listKeys) :

        f.write('%s"%s" : {\n' % (prefix, stmt.arg))

        self.__emit_patch_stmt_body(f, stmt, level + 1, listKeys)

        f.write('%s}' % (prefix))

        if peerAcc > 1 :
            f.write(',')

        f.write('\n')

    def __emit_patch_stmt_body(self, f, stmt, level, listKeys) :
        ss = self._get_expanded_body_stmts(stmt.substmts)

        pa = len(ss)
        for s in ss :
            self.__emit_stmt(f, s, level, pa, listKeys = listKeys)
            pa -= 1


    def __emit_patch_leaf(self, f, prefix, stmt, peerAcc, listKeys) :
        t = self.rh.get_type_stmt(stmt).arg

        v = None
        if listKeys and stmt.parent and (stmt.parent.keyword == 'list') and (stmt.arg in listKeys) :
            v = listKeys[stmt.arg]

        if t == 'string' :
            if v == None :
                v = '%s value' % (stmt.arg)

            f.write('%s"%s" : "%s"' % (prefix, stmt.arg, v))
        elif t.startswith('int') or t.startswith('uint') :
            if v == None :
                v = 42

            f.write('%s"%s" : %s' % (prefix, stmt.arg, v))
        else :
            f.write('%s"%s" : "..."' % (prefix, stmt.arg))

        if peerAcc > 1 :
            f.write(',')

        f.write('\n')

    def __emit_patch_key_leaf(self, f, level, stmt, peerAcc, listKeys) :
        prefix = self.__get_prefix(level)
        t = self.rh.get_type_stmt(stmt).arg

        v = None
        if listKeys and stmt.parent and (stmt.parent.keyword == 'list') and (stmt.arg in listKeys) :
            v = listKeys[stmt.arg]

        if t == 'string' :
            if v == None :
                v = self.rh.get_key_name(stmt.arg)

            f.write('%s"%s" : "%s"' % (prefix, stmt.arg, v))
        elif t.startswith('int') or t.startswith('uint') :
            # FIXME : Value for patch integer list key

            if v == None :
                v = 42

            f.write('%s"%s" : %s' % (prefix, stmt.arg, v))

        else :
            f.write('%s"%s" : "..."' % (prefix, stmt.arg))

        if peerAcc > 1 :
            f.write(',')

        f.write('\n')


    def __emit_patch_list(self, f, prefix, stmt, level, listKeys) :
        nsh = NcsServiceHelper()
        ignoreServiceStmt = nsh.stmt_contains_service(stmt)

        keys = self.rh.get_list_keys(stmt)
        f.write('%s"%s" : {\n' % (prefix, stmt.arg))

        ss = self._get_expanded_body_stmts(stmt.substmts, ['key'])

        pa = len(ss)

        for s in ss :
            if s.keyword == 'key' :
                pa -= 1

        for s in ss :
            skw = s.keyword

            if skw == 'key' :
                # 'key' not in output
                pass
            elif (skw == 'leaf') and (s.arg in keys) :
                self.__emit_patch_key_leaf(f, level + 1, s, pa, listKeys)
                pa -= 1
            elif ignoreServiceStmt and nsh.is_service_stmt(s) :
                # Ignore service stmt
                pass
            else :
                self.__emit_stmt(f, s, level + 1,  pa, listKeys)
                pa -= 1

        f.write('%s}\n' % (prefix))

    def __emit_patch_choice(self, f, stmt, level, peerAcc, listKeys) :
        peerAcc += len(stmt.substmts) - 1
        for s in stmt.substmts :
            self.__emit_stmt(f, s, level, peerAcc, listKeys),
            peerAcc -= 1

    def __emit_patch_case(self, f, stmt, level, peerAcc, listKeys) :
        for s in stmt.substmts :
            self.__emit_stmt(f, s, level, peerAcc, listKeys)


# ------------------------------------------------------------------------------

class RestCurlHelper(object) :

    def __init__(self) :
        self.rh = RestHelper()


    def emit_curl_get_query_type(self, f, address, accept, user, db, stmt,
                              lastListKeys = False, urlEncode = False,
                              listKeys = None, queryType = None,
                              newLine = False, pathWithPrefix = False,
                              curlFlags = None) :

        cs = self.get_curl_GET_str(address, accept, user, db, stmt,
                                       lastListKeys = lastListKeys,
                                       urlEncode = urlEncode,
                                       listKeys = listKeys,
                                       newLine = newLine,
                                       pathWithPrefix = pathWithPrefix,
                                       curlFlags = curlFlags)

        if not queryType or (queryType == 'default') :
            f.write('%s\n' % (cs))
        else :
            f.write('%s?%s\n' % (cs, queryType))


    def emit_curl_delete(self, f, address, accept, user, db, stmt,
                         lastListKeys = False, urlEncode = False,
                         listKeys = None, newLine = False,
                         pathWithPrefix = False,
                         curlFlags = None) :

        cs = self.get_curl_DELETE_str(address, accept, user, db, stmt,
                                       lastListKeys = lastListKeys,
                                       urlEncode = urlEncode,
                                       listKeys = listKeys,
                                       newLine = newLine,
                                       pathWithPrefix = pathWithPrefix,
                                       curlFlags = curlFlags)

        f.write('%s\n' % (cs))


    def __get_curl_verb_str(self, verb, address, accept, user, db, stmt,
                           query = None,
                           lastListKeys = False, urlEncode = False,
                           listKeys = None, newLine = False,
                           pathWithPrefix = False, curlFlags = None) :
        rh = self.rh

        path = rh.rest_api_path(db, stmt, query = query,
                                lastListKeys = lastListKeys,
                                urlEncode = urlEncode,
                                listKeys = listKeys,
                                pathWithPrefix = pathWithPrefix)
        path = address + path
        accept = accept

        # -v Verbose
        # -i Include the HTTP header in the output
        ret = 'curl -X ' + verb

        if curlFlags :
            ret += ' ' + curlFlags

        if newLine :
            ret += ' \\\n '

        ret += ' -u ' + user + ' '


        if newLine :
            ret += '\\\n  '

        ret += '-H "Accept: ' + accept + '" '


        if newLine :
            ret += '\\\n  '

        ret += path

        return ret


    def get_curl_GET_str(self, address, accept, user, db, stmt, query = None,
                         lastListKeys = False, urlEncode = False,
                         listKeys = None, newLine = False,
                         pathWithPrefix = False,
                         curlFlags = None) :

        return self.__get_curl_verb_str('GET', address, accept, user, db, stmt,
                                        query = query,
                                        lastListKeys = lastListKeys,
                                        urlEncode = urlEncode,
                                        listKeys= listKeys,
                                        newLine = newLine,
                                        pathWithPrefix = pathWithPrefix,
                                        curlFlags = curlFlags)

    def get_curl_ppp_str(self, verb, address, contentType, user, db, stmt,
                         dataFromStdin = False, newLine = False,
                         pathWithPrefix = False, curlFlags = None,
                         dropLastListInPath = False) :
        rh = self.rh

        path = rh.rest_api_path(db, stmt,
                                pathWithPrefix = pathWithPrefix,
                                dropLastListInPath = dropLastListInPath)
        path = address + path

        # -v Verbose
        # -i Include the HTTP header in the output

        ret = 'curl '

        if curlFlags :
            ret += curlFlags + ' '

        ret += '-X %s ' % (verb)

        if dataFromStdin :
            ret += '-d @- '

        if newLine :
            ret += '\\\n -u ' + user + ' '
        else :
            ret += '-u ' + user + ' '

        if newLine :
            ret += '\\\n '

        ret += '-H "Content-type: ' + contentType + '" '

        if newLine :
            ret += '\\\n '

        ret += path

        return ret


    def get_curl_DELETE_str(self, address, accept, user, db, stmt, query = None,
                         lastListKeys = False, urlEncode = False,
                         listKeys = None, newLine = False,
                         pathWithPrefix = False,
                         curlFlags = None) :

        return self.__get_curl_verb_str('DELETE', address, accept, user, db,
                                        stmt,
                                        query = query,
                                        lastListKeys = lastListKeys,
                                        urlEncode = urlEncode,
                                        listKeys = listKeys,
                                        newLine = newLine,
                                        pathWithPrefix = pathWithPrefix,
                                        curlFlags = curlFlags)


# ------------------------------------------------------------------------------

class RestContainerEmitter(object) :

    def __init__(self) :
        self.rh = RestHelper()

    def emit_get(self, f, db, containerStmt, query, pathWithPrefix = False,
                showDescription = False) :
        rgh = RestGetHelper()

        rgh.emit_get_begin(f, containerStmt)

        if (showDescription) :
            rgh.emit_get_description(f, containerStmt)

        rgh.emit_get(f, db, containerStmt, query,
                     pathWithPrefix = pathWithPrefix)

        rgh.emit_get_headers(f)
        rgh.emit_get_end(f)

    def emit_ppp(self, verb, f, db, containerStmt, pathWithPrefix, showDescription) :
        rph = RestPPPHelper()

        rph.emit_ppp_begin(verb, f, containerStmt)

        if showDescription :
            rph.emit_ppp_description(f, containerStmt)

        rph.emit_ppp(verb, f, db, containerStmt,
                     pathWithPrefix = pathWithPrefix)

        rph.emit_ppp_end(verb, f)

    def emit_delete(self, f, db, containerStmt,
                    pathWithPrefix = False, showDescription = False) :
        rdh = RestDeleteHelper()

        rdh.emit_delete_begin(f, containerStmt)

        if showDescription :
            rdh.emit_delete_description(f, containerStmt)

        rdh.emit_delete(f, db, containerStmt, pathWithPrefix = pathWithPrefix)
        rdh.emit_delete_headers(f)
        rdh.emit_delete_end(f)


    def emit_curl_ppp(self, verb, f, address, contentType, user, db, stmt,
                        newLine = False,
                        pathWithPrefix = False,
                        listKeys = None, curlFlags = None) :

        rph = RestPPPHelper()
        rph.emit_ppp_curl(verb, f, address, contentType, user, db, stmt,
                          listKeys = listKeys,
                          newLine = newLine,
                          pathWithPrefix = pathWithPrefix,
                          curlFlags = curlFlags)


    def emit_curl_get_query_type(self, f, address, accept, user, db,
                                 containerStmt, queryType,
                                 newLine = False,
                                 pathWithPrefix = False,
                                 curlFlags = None) :
        rch = RestCurlHelper()
        rch.emit_curl_get_query_type(f, address, accept, user, db,
                                     containerStmt,
                                     queryType = queryType, newLine = newLine,
                                     pathWithPrefix = pathWithPrefix,
                                     curlFlags = curlFlags)

    def emit_curl_delete(self, f, address, accept, user, db, containerStmt,
                         newLine = False, pathWithPrefix = False,
                         curlFlags = None) :
        rch = RestCurlHelper()
        rch.emit_curl_delete(f, address, accept, user, db, containerStmt,
                             newLine = newLine,
                             pathWithPrefix = pathWithPrefix,
                             curlFlags = curlFlags)


# ------------------------------------------------------------------------------

class RestListEmitter(object) :

    def __init__(self) :
        self.rh = RestHelper();

    def emit_get(self, f, db, listStmt, query, pathWithPrefix, showDescription) :
        rgh = RestGetHelper()

        rgh.emit_get_begin(f, listStmt)

        if showDescription :
            rgh.emit_get_description(f, listStmt)

        rgh.emit_get(f, db, listStmt, query, pathWithPrefix = pathWithPrefix)
        rgh.emit_get(f, db, listStmt, query,
                     lastListKeys = True, pathWithPrefix = pathWithPrefix)

        rgh.emit_get_headers(f, isCollection = True)
        rgh.emit_get_end(f)


    def emit_delete(self, f, db, listStmt, pathWithPrefix, showDescription) :
        rdh = RestDeleteHelper()

        rdh.emit_delete_begin(f, listStmt)

        if showDescription :
            rdh.emit_delete_description(f, listStmt)

        rdh.emit_delete(f, db, listStmt, pathWithPrefix = pathWithPrefix)
        rdh.emit_delete(f, db, listStmt,
                        lastListKeys = True,
                        pathWithPrefix = pathWithPrefix)

        rdh.emit_delete_headers(f)
        rdh.emit_delete_end(f)


    def emit_curl_get_query_type(self, f, address, accept, user, db, listStmt,
                              listKeys, endListUrlWithKeys, queryType,
                              newLine = False, pathWithPrefix = False,
                              curlFlags = None) :
        rch = RestCurlHelper()
        rch.emit_curl_get_query_type(f, address, accept, user, db, listStmt,
                                  lastListKeys = endListUrlWithKeys,
                                  urlEncode = True,
                                  listKeys = listKeys,
                                  queryType = queryType,
                                  newLine = newLine,
                                  pathWithPrefix = pathWithPrefix,
                                  curlFlags = curlFlags)


    def emit_curl_delete(self, f, address, accept, user, db, listStmt,
                         listKeys, endListUrlWithKeys, newLine = False,
                         pathWithPrefix = False, curlFlags = None) :

        rch = RestCurlHelper()
        rch.emit_curl_delete(f, address, accept, user, db, listStmt,
                             lastListKeys = endListUrlWithKeys,
                             urlEncode = True,
                             listKeys = listKeys,
                             newLine = newLine,
                             pathWithPrefix = pathWithPrefix,
                             curlFlags = curlFlags)


    def emit_ppp(self, verb, f, db, listStmt, pathWithPrefix,
                 dropLastListInPath = False,
                 showDescription = False) :

        rph = RestPPPHelper()

        rph.emit_ppp_begin(verb, f, listStmt)

        if showDescription :
            rph.emit_ppp_description(f, listStmt)

        if dropLastListInPath :
            rph.emit_ppp_url(verb, f, db, listStmt,
                             urlEncode = False,
                             pathWithPrefix = pathWithPrefix,
                             dropLastListInPath = True)

        else :
            rph.emit_ppp_url(verb, f, db, listStmt,
                             lastListKeys = True, urlEncode = False,
                             pathWithPrefix = pathWithPrefix)

        rph.emit_ppp_headers(f)
        rph.emit_ppp_body(f, listStmt)

        rph.emit_ppp_end(verb, f)

    def emit_ppp_curl(self, verb, f, address, contentType, user, db, stmt,
                      listKeys = None,
                      newLine = False,
                      pathWithPrefix = False,
                      curlFlags = None,
                      dropLastListInPath = False) :

        rph = RestPPPHelper()
        rph.emit_ppp_curl(verb, f, address, contentType, user,
                          db, stmt, listKeys,
                          newLine = newLine,
                          pathWithPrefix = pathWithPrefix,
                          curlFlags = curlFlags,
                          dropLastListInPath = dropLastListInPath)



# ------------------------------------------------------------------------------

class DocBookHelper(object) :

    def __init__(self) :
        self.level = 0

    def __prefix(self) :
        return ''.rjust(self.level * 2)

    def __write(self, f, txt) :
        f.write(txt)

    def __write_prefix(self, f, txt) :
        prefix = self.__prefix()
        f.write('%s%s' % (prefix, txt))

    def __writeln(self, f, txt) :
        if txt != None :
            self.__write_prefix(f, txt)

        f.write('\n')

    def write(self, f, txt) :
        self.__write(f, txt)

    def writeln(self, f, txt = None) :
        self.__writeln(f, txt)


    def __inc_level(self) :
        self.level += 1

    def __dec_level(self) :
        self.level -= 1

    def begin(self, f, tag) :
        self.writeln(f, '<' + tag + '>')
        self.__inc_level()

    def begin_(self, f, tag) :
        self.__write_prefix(f, '<' + tag + '>')


    def end(self, f, tag) :
        self.__dec_level()
        self.writeln(f, '</' + tag + '>')

    def end_(self, f, tag) :
        self.__write(f, '</' + tag + '>\n')


    def document_begin(self, f) :
        self.__writeln(f, '<?xml version="1.0"?>')
        self.__chapter_begin(f)

    def document_end(self, f) :
        self.__chapter_end(f)

    def sect_begin(self, f, level = 1) :
        self.begin(f, 'sect%s' % (level))

    def sect_end(self, f, level = 1) :
        self.end(f, 'sect%s' % (level))


    def title(self, f, txt) :
        self.writeln(f, '<title>%s</title>' % (txt))

    def para(self, f, txt) :
        self.begin(f, 'para')
        self.writeln(f, txt)
        self.end(f,'para')

    def para_(self, f, txt) :
        self.begin_(f, 'para')
        self.__write(f, txt)
        self.end_(f,'para')


    def __chapter_begin(self, f) :
        self.writeln(f, '<chapter xmlns="http://docbook.org/ns/docbook" version="5.0"')
        self.writeln(f, '         xmlns:xi="http://www.w3.org/2001/XInclude"')
        self.writeln(f, '         xml:id="ug.ncs_sysmgmt"')
        self.writeln(f, '         xmlns:xlink="http://www.w3.org/1999/xlink">')
        self.writeln(f)

        self.__inc_level()

    def __chapter_end(self, f) :
        self.__dec_level()
        self.writeln(f, '</chapter>')




# ------------------------------------------------------------------------------

def update_context_accept_and_content_type_from_stmt(ctx, stmt) :
    ot = ctx.opts.outputType
    listKeys = ctx.opts.listKeys

    if ctx.opts.restAcceptValue == '<default>' :
        ctx.opts.restAccept = 'application/vnd.yang.data+json'

        if (ot == 'get') or (ot == 'get_curl') :
            if (stmt.keyword == 'list') and not listKeys :
                ctx.opts.restAccept = 'application/vnd.yang.collection+json'

    else :
        ctx.opts.restAccept = ctx.opts.restAcceptValue

    ctx.opts.restContentType = ctx.opts.restAccept

class RestDocEmitter(object) :

    def __init__(self) :
        self.sh = StmtHelper()

    def emit(self, ctx, modules, f) :
        if ctx.opts.outputType == 'docbook' :
            self.__emit_docbook(ctx, modules, f)
        else :
            self.__emit_verb(ctx, modules, f)

    def __emit_docbook(self, ctx, modules, f) :
        dbh = DocBookHelper()
        dbh.document_begin(f)
        dbh.title(f, 'REST Documentation')

        dbh.sect_begin(f)
        dbh.title(f, 'Overview')

        cmdLine = ''
        for arg in sys.argv[1:] :
            if cmdLine :
                cmdLine += ' '

            cmdLine += arg

        dbh.para(f, 'pyang %s' % (cmdLine))
        dbh.sect_end(f)

        #dbh.sect_begin(f)
        #dbh.title(f, 'Elements')

        for module in modules :
            self.__emit_docbook_module(dbh, ctx, f, module, sectLevel = 1)

        #dbh.sect_end(f)

        dbh.document_end(f)


    def __emit_verb(self, ctx, modules, f) :

        o = ctx.opts

        if o.stmts :
            # One specific statement

            sp = o.stmtPath

            nonPrefixPaths = []
            for stmt in o.stmts :
                nonPrefixPaths.append(mk_path_str(stmt, False))

            for stmt, npPath in zip(o.stmts, nonPrefixPaths) :
                pathWithPrefix = False

                if o.stmtPath.find(':') >= 0 :
                    # Specified path contains prefixes, use that in output
                    pathWithPrefix = True

                if self.__is_emit_one_stmt(stmt) and (nonPrefixPaths.count(npPath) > 1) :
                    # Duplicate non-prefixed paths, enforce path prefix
                    pathWithPrefix = True

                self.__emit_one_stmt(ctx, o.outputType, stmt, f,
                                     pathWithPrefix, o.showDescription)

        else :
            # All statements
            for module in modules :
                self.__emit_all_sub_statements(ctx, module, f, o.iterLevel + 1,
                                              showDescription = o.showDescription)

    # --------------------------------------------------------------------------

    def __emit_docbook_module(self, docBookHelper, ctx, f, module,
                              sectLevel = 2) :
        dbh = docBookHelper
        o = ctx.opts

        moduleName = module.i_modulename

        dbh.sect_begin(f, sectLevel)
        dbh.title(f, 'Module %s' % (moduleName))

        self.__emit_docbook_stmts(dbh, ctx, f, module.substmts, sectLevel + 1)

#        self.__emit_docbook_all_substmts(dbh, ctx, f, module,
#                                              o.iterLevel + 1,
#                                              sectLevel + 1)

        dbh.sect_end(f, sectLevel)


    def __emit_docbook_stmts(self, docbookHelper, ctx, f, stmts, sectLevel) :
        for s in stmts :
            if not self.sh.is_unknown_stmt(s) and (s.keyword in ['list', 'container']) :
                self.__emit_docbook_stmt(docbookHelper, ctx, f, s, sectLevel)


    def __emit_docbook_all_substmts(self, docBookHelper, ctx, f, stmt,
                                         level, sectLevel) :
        if level <= 0 :
            return

        emitStmts = ['module', 'container', 'list', 'leaf']
        if not stmt.keyword in emitStmts :
            return

        dbh = docBookHelper

        if stmt.keyword != 'module' :
            self.__emit_docbook_stmt(dbh, ctx, f, stmt, sectLevel)

        for ss in stmt.substmts:
            self.__emit_docbook_all_substmts(dbh, ctx, f, ss, level-1,
                                                 sectLevel)

    def __emit_docbook_stmt(self, docBookHelper, ctx, f, stmt, sectLevel) :
        sh = StmtHelper()
        dbh = docBookHelper

        update_context_accept_and_content_type_from_stmt(ctx, stmt);

        dbh.sect_begin(f, sectLevel)
        title = '%s' % (sh.get_plain_path(stmt))
        dbh.title(f,title)


        orgCurlNewLine = ctx.opts.curlNewLine
        ctx.opts.curlNewLine = True

        dbh.para(f, 'GET')
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'get', f, stmt)
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'get_curl', f, stmt)

        dbh.para(f,'')
        dbh.para(f, 'PUT')
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'put', f, stmt)
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'put_curl', f, stmt)

        dbh.para(f,'')
        dbh.para(f, 'POST')
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'post', f, stmt)
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'post_curl', f, stmt)


        dbh.para(f,'')
        dbh.para(f, 'PATCH')
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'patch', f, stmt)
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'patch_curl', f, stmt)

        dbh.para(f,'')
        dbh.para(f, 'DELETE')
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'delete', f, stmt)
        self.__emit_docbook_stmt_plain_para(dbh, ctx, 'delete_curl', f, stmt)

        ctx.opts.curlNewLine = orgCurlNewLine

        dbh.sect_end(f, sectLevel)


    def __emit_docbook_stmt_plain_para(self, docBookHelper, ctx, outputType,
                                       f, stmt) :
        dbh = docBookHelper

        dbh.begin(f, 'informalexample')
        dbh.begin_(f, 'programlisting')

        o = ctx.opts

        fs = StringIO.StringIO()

        self.__emit_one_stmt(ctx, outputType, stmt, fs,
                             pathWithPrefix = False,
                             showDescription = o.showDescription)

        dbh.write(f, self.__xml_escape(fs.getvalue().strip()))
        fs.close()


        dbh.end_(f, 'programlisting')
        dbh.end(f, 'informalexample')


    def __xml_escape(self, txt) :
        txt = txt.replace('&', '&amp;')
        txt = txt.replace('<', '&lt;')
        txt = txt.replace('>', '&gt;')
        txt = txt.replace('\'', '&apos;')
        txt = txt.replace('"', '&quot;')

        return txt

    # --------------------------------------------------------------------------

    def __emit_all_sub_statements(self, ctx, stmt, f, level,
                                  pathWithPrefix = False,
                                  showDescription = False) :
        if level <= 0 :
            return

        self.__emit_one_stmt(ctx, ctx.opts.outputType, stmt, f,
                             pathWithPrefix, showDescription)

        if stmt.substmts :
            for s in stmt.substmts :
                self.__emit_all_sub_statements(ctx, s, f, level - 1,
                                               pathWithPrefix, showDescription)

    def __is_emit_one_stmt(self, stmt) :
        return stmt.keyword in ['list', 'container', 'leaf']

    def __emit_one_stmt(self, ctx, outputType, stmt, f, pathWithPrefix, showDescription) :
        update_context_accept_and_content_type_from_stmt(ctx, stmt);

        ot = outputType


        if stmt.keyword == 'list' :
            self.__emit_one_list_stmt(ctx, ot, stmt, f,
                                      pathWithPrefix, showDescription)
        elif stmt.keyword == 'container' :
            self.__emit_one_container_stmt(ctx, ot, stmt, f,
                                           pathWithPrefix, showDescription)
        elif stmt.keyword == 'leaf' :
            self.__emit_one_leaf_stmt(ctx, ot, stmt, f,
                                      pathWithPrefix, showDescription)
        else :
            pass


    def __emit_one_container_stmt(self, ctx, outputType, stmt, f,
                                  pathWithPrefix, showDescription) :
        rce = RestContainerEmitter()

        o = ctx.opts
        ot = outputType

        # NOTE: No description for curl variants

        if ot == 'get' :
            rce.emit_get(f, o.db, stmt, o.queryType,
                         pathWithPrefix = pathWithPrefix,
                         showDescription = showDescription)

        elif ot == 'get_curl' :
            rce.emit_curl_get_query_type(f, o.restAddress, o.restAccept,
                                         o.restUser, o.db, stmt, o.queryType,
                                         newLine = o.curlNewLine,
                                         pathWithPrefix = pathWithPrefix,
                                         curlFlags = o.curlFlags)

        elif ot == 'delete' :
            rce.emit_delete(f, o.db, stmt,
                            pathWithPrefix = pathWithPrefix,
                            showDescription = showDescription)
        elif ot == 'delete_curl' :
            rce.emit_curl_delete(f, o.restAddress, o.restAccept,
                                 o.restUser, o.db, stmt,
                                 newLine = o.curlNewLine,
                                 pathWithPrefix = pathWithPrefix,
                                 curlFlags = o.curlFlags)

        else :
            isCurl = False

            if ot.startswith('patch') :
                verb = 'PATCH'
            elif ot.startswith('put') :
                verb = 'PUT'
            elif ot.startswith('post') :
                if ot == 'post' :
                    rh = RestHelper()
                    f.write('# %s\n' % (rh.get_one_line_stmt_info(stmt)))

                if stmt.keyword == 'leaf' :
                    f.write('POST : Not supported on a leaf statement\n')
                else :
                    f.write('POST : Not supported on a container statement\n')

                return

            if ot.endswith('_curl') :
                isCurl = True

            if isCurl :
                rce.emit_curl_ppp(verb, f, o.restAddress, o.restContentType,
                                    o.restUser, o.db, stmt,
                                    newLine = o.curlNewLine,
                                    pathWithPrefix = pathWithPrefix,
                                    listKeys = o.listKeys,
                                    curlFlags = o.curlFlags)
            else :
                rce.emit_ppp(verb, f, o.db, stmt, pathWithPrefix, showDescription)

    def __emit_one_list_stmt(self, ctx, outputType, stmt, f, pathWithPrefix, showDescription) :
        rle = RestListEmitter()

        o = ctx.opts
        ot = outputType

        if ot == 'get' :
            rle.emit_get(f, o.db, stmt, o.queryType,
                         pathWithPrefix, showDescription)
        elif ot == 'get_curl' :
            self.__emit_list_get_curl(rle, f, ot, o, stmt,
                                      newLine = o.curlNewLine,
                                      pathWithPrefix = pathWithPrefix,
                                      curlFlags = o.curlFlags)

        elif ot == 'patch' :
            rle.emit_ppp('PATCH', f, o.db, stmt, pathWithPrefix,
                        showDescription = showDescription)
        elif ot == 'patch_curl' :
            rle.emit_ppp_curl('PATCH', f, o.restAddress, o.restContentType,
                                o.restUser, o.db, stmt, o.listKeys,
                                newLine = o.curlNewLine,
                                pathWithPrefix = pathWithPrefix,
                                curlFlags = o.curlFlags)

        elif ot == 'delete' :
            rle.emit_delete(f, o.db, stmt, pathWithPrefix, showDescription)
        elif ot == 'delete_curl' :
            self.__emit_list_delete_curl(rle, f, ot, o, stmt,
                                        newLine = o.curlNewLine,
                                        pathWithPrefix = pathWithPrefix,
                                        curlFlags = o.curlFlags)

        elif (ot == 'put') or (ot == 'put_curl') :
            if ot == 'put' :
                rh = RestHelper()
                f.write('# %s\n' % (rh.get_one_line_stmt_info(stmt)))

            f.write('PUT : Not supported on a list statement\n')

        else :
            isCurl = False
            dropLastListInPath = False

            if ot.startswith('patch') :
                verb = 'PATCH'
            elif ot.startswith('put') :
                verb = 'PUT'
            elif ot.startswith('post') :
                verb = 'POST'
                dropLastListInPath = True

            if ot.endswith('_curl') :
                isCurl = True

            if isCurl :
                rle.emit_ppp_curl(verb, f, o.restAddress, o.restContentType,
                                  o.restUser, o.db, stmt, o.listKeys,
                                  newLine = o.curlNewLine,
                                  pathWithPrefix = pathWithPrefix,
                                  curlFlags = o.curlFlags,
                                  dropLastListInPath = dropLastListInPath)
            else :
                rle.emit_ppp(verb, f, o.db, stmt, pathWithPrefix,
                             dropLastListInPath = dropLastListInPath,
                             showDescription = showDescription)

    def __emit_list_get_curl(self, rle, f, outputType, opts, stmt,
                             newLine, pathWithPrefix, curlFlags) :
        ot = outputType
        o = opts

        rle.emit_curl_get_query_type(f, o.restAddress, o.restAccept,
                                  o.restUser, o.db, stmt,
                                  listKeys = o.listKeys,
                                  endListUrlWithKeys = o.endListUrlWithKeys,
                                  queryType = o.queryType,
                                  newLine = newLine,
                                  pathWithPrefix = pathWithPrefix,
                                  curlFlags = curlFlags)

    def __emit_list_delete_curl(self, rle, f, outputType, opts, stmt,
                               newLine = False, pathWithPrefix = False,
                               curlFlags = None) :
        ot = outputType
        o = opts

        rle.emit_curl_delete(f, o.restAddress, o.restAccept,
                             o.restUser, o.db, stmt,
                             listKeys = o.listKeys,
                             endListUrlWithKeys = o.endListUrlWithKeys,
                             newLine = newLine,
                             pathWithPrefix = pathWithPrefix,
                             curlFlags = curlFlags)


    def __emit_one_leaf_stmt(self, ctx, outputType, stmt, f, pathWithPrefix,
                             showDescription) :
        self.__emit_one_container_stmt(ctx, outputType, stmt, f,
                                       pathWithPrefix, showDescription)


