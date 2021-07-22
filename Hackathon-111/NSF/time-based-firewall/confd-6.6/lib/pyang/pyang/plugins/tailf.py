"""Tail-f vendor-specific extensions plugin"""

import optparse
import re
import copy
import sys

import pyang
from pyang import plugin
from pyang import syntax
from pyang import grammar
from pyang import error
from pyang import statements
from pyang import types
from pyang import util
from pyang import xpath
from pyang.error import err_add
from pyang.util import attrsearch

tailf = 'tailf-common'
tailf_ncs = 'tailf-ncs'
yang = 'ietf-yang-types'

def pyang_plugin_init():
    # register the plugin
    plugin.register_plugin(TailfPlugin())

    statements.add_data_keyword((tailf, 'symlink'))
    statements.add_data_keyword((tailf, 'action'))
    statements.add_keyword_with_children((tailf, 'action'))
    statements.add_keywords_with_no_explicit_config((tailf, 'action'))
    statements.add_data_keyword((tailf, 'error-info'))
    statements.add_keyword_with_children((tailf, 'error-info'))
    statements.add_keywords_with_no_explicit_config((tailf, 'error-info'))

    # register tail-f's xpath functions
    statements.add_xpath_function('re-match')
    statements.add_xpath_function('string-compare')
    statements.add_xpath_function('compare')
    statements.add_xpath_function('min')
    statements.add_xpath_function('max')
    statements.add_xpath_function('avg')
    statements.add_xpath_function('band')
    statements.add_xpath_function('bor')
    statements.add_xpath_function('bxor')
    statements.add_xpath_function('bnot')


    statements.add_validation_phase('tailf_pre_import', before='import')
    statements.add_validation_fun('tailf_pre_import',
                                  ['module', 'submodule'],
                                  lambda ctx, s: v_pre_import(ctx, s))
    # do annotate-statement before expansion
    statements.add_validation_phase('tailf_ann', before='expand_1')
    # do annotate after expansion
    statements.add_validation_phase('tailf_ann2', after='expand_2')
    statements.add_validation_phase('tailf_checks', before='unused')

    statements.add_validation_fun('tailf_checks',
                                  [(tailf, 'path')],
                                  lambda ctx, s: v_chk_path(ctx, s))
    statements.add_validation_fun('tailf_ann',
                                  ['module', 'submodule'],
                                  lambda ctx, s: v_annotate_module(ctx, s))
    statements.add_validation_fun('tailf_ann2',
                                  ['module'],
                                  lambda ctx, s: v_annotate(ctx, s))
    statements.add_validation_fun('tailf_checks',
                                  [(tailf, 'annotate')],
                                  lambda ctx, s: v_chk_annotate(ctx, s))


class TailfPlugin(plugin.PyangPlugin):
    def setup_ctx(self, ctx):
        ctx.tailf_ann_mods = {}
        ctx.tailf_ann_files = []

    def add_opts(self, optparser):
        optlist = [
            optparse.make_option("-a", "--annotate",
                                 metavar="FILENAME",
                                 dest="tailf_annotate",
                                 default=[],
                                 action="append",
                                 help="Module with annotations"),
            ]
        sanitize_optlist = [
            optparse.make_option("--tailf-sanitize",
                                 dest="tailf_sanitize",
                                 action="store_true",
                                 help="Remove tailf-specific annotations"),
            optparse.make_option("--tailf-remove-body",
                                 dest="tailf_remove_body",
                                 action="store_true",
                                 help="Keep only typedefs and groupings " \
                                 "in sanitation"),
            optparse.make_option("--tailf-keep-non-std-xpath",
                                 dest="tailf_keep_non_std_xpath",
                                 action="store_true",
                                 help="Keep must and when with non-standard "\
                                     "XPath functions in sanitation"),
            optparse.make_option("--tailf-keep-actions",
                                 dest="tailf_keep_actions",
                                 action="store_true",
                                 help="Keep tailf:action in sanitation"),
            optparse.make_option("--tailf-keep-dependency",
                                 dest="tailf_keep_dependency",
                                 action="store_true",
                                 help="Keep tailf:dependency in sanitation"),
            optparse.make_option("--tailf-keep-info",
                                 dest="tailf_keep_info",
                                 action="store_true",
                                 help="Keep tailf:info in sanitation"),
            optparse.make_option("--tailf-keep-tailf-typedefs",
                                 dest="tailf_keep_typedefs",
                                 action="store_true",
                                 help="Keep types from tailf-common " \
                                 "in sanitation"),
            optparse.make_option("--tailf-remove-symlink",
                                 dest="tailf_remove_symlink",
                                 action="store_true",
                                 help="Remove tailf:symlink in sanitation"),
            optparse.make_option("--tailf-keep-symlink-when",
                                 dest="tailf_keep_when",
                                 action="store_true",
                                 help="Keep when statements in symlinks " \
                                 "in sanitation"),
            optparse.make_option("--tailf-keep-symlink-must",
                                 dest="tailf_keep_must",
                                 action="store_true",
                                 help="Keep must statements in symlinks " \
                                 "in sanitation"),
            optparse.make_option("--tailf-keep-display-when",
                                 dest="tailf_keep_display_when",
                                 action="store_true",
                                 help="Keep tailf:display-when statements " \
                                 "in sanitation"),
            ]
        if hasattr(optparser, 'tailf_opts'):
            g = optparser.tailf_opts
        else:
            g = optparser.add_option_group("Tail-f specific options")
            optparser.tailf_opts = g
        g.add_options(optlist)
        if hasattr(optparser, 'tailf_sanitize_opts'):
            g = optparser.tailf_sanitize_opts
        else:
            g = optparser.add_option_group("Tail-f Sanitation options")
            optparser.tailf_sanitation_opts = g
        g.add_options(sanitize_optlist)
        self.mods = []

    def pre_load_modules(self, ctx):
        def pre_load_file(filename):
            try:
                ffd = file(filename)
                text = ffd.read()
            except IOError:
                _, ex, _ = sys.exc_info()
                sys.stderr.write("error %s: %s\n" % (filename, str(ex)))
                sys.exit(1)
            # add the module, but abort the validation early
            # (see tailf.v_pre_import())
            ctx.tailf_ann_files.append(filename)
            m = ctx.add_module(filename, text)
            if m is not None:
                if m.keyword == 'module':
                    self.mods.append(m.arg)
                ctx.tailf_ann_mods[filename] = (m, False)
                for i in m.search('include'):
                    subm = ctx.read_module(i.arg)
                    if subm is not None:
                        pre_load_file(subm.pos.ref)

        # load annotation modules and their submodules
        for filename in ctx.opts.tailf_annotate:
            pre_load_file(filename)

    def pre_validate_ctx(self, ctx, modules):
        if len(modules) == 0:
            return
        module = modules[0]
        self.mods.extend([module.arg] +
                         [i.arg for i in module.search('include')])
        # make sure validate_module continues for the annotation modules
        # (see tailf.v_pre_import())
        ctx.tailf_ann_files = []

        # apply annotations
        for f in ctx.tailf_ann_mods:
            (m, validated) = ctx.tailf_ann_mods[f]
            if not validated:
                statements.validate_module(ctx, m)
            # add any imports used by the annotation module
            # to the main module, so that -f yang (etc) works
            for s in m.search('import'):
                if (s.arg != module.arg and
                    module.search_one('import', s.arg) is None):
                    module.substmts.insert(
                        module.substmts.index(module.search_one('prefix'))+1, s)

    def post_validate_ctx(self, ctx, modules):
        if not ctx.opts.tailf_sanitize:
            return
        for (epos, etag, eargs) in ctx.errors:
            if (epos.top in modules and
                error.is_error(error.err_level(etag))):
                return

        for m in modules:
            sanitize(ctx, m)


def v_chk_path(ctx, s):
    path_spec = types.validate_path_expr(ctx.errors, s)
    if ctx.opts.tailf_sanitize:
        # when we do sanitization, we require symlink paths to point
        # to nodes we know, so that we can copy them

        # Symlinks should *inherit* config from target node, so don't
        # object against config mismatch.
        if (s.parent.keyword[1] == 'symlink'):
            accept_nct = True
        else:
            accept_nct = False
        x = statements.validate_leafref_path(
                ctx, s.parent, path_spec, s,
                accept_non_leaf_target = True,
                accept_non_config_target = accept_nct)
        if x is None:
            s.tailf_target_node = None
            return
        ptr, expanded_path, path_list = x
        s.tailf_target_node = ptr

def v_pre_import(ctx, s):
    # If this is an annotation module, abort validation at this point.
    # Validation of this module will be done later, by cs.py.
    if s.pos.ref in ctx.tailf_ann_files:
        return 'stop'

def v_annotate_module(ctx, s):
    # called for each module/submodule loaded before expansion
    # check if the module/submodule is annotated
    for f in ctx.tailf_ann_mods:
        (m, _validated) = ctx.tailf_ann_mods[f]
        am = m.search_one((tailf, 'annotate-module'), s.arg)
        if am is not None:
            am.i_annotate_node = s
            for substmt in am.substmts:
                if substmt.keyword == (tailf, 'annotate-statement'):
                    v_annotate_statement(ctx, substmt)
                else:
                    s.substmts.append(substmt)

def v_annotate_statement(ctx, s):
    if not s.is_grammatically_valid:
        return
    if not hasattr(s.parent, 'i_annotate_node'):
        err_add(ctx.errors, s.pos, 'TAILF_BAD_ANNOTATE', ())
        return None
    node = s.parent.i_annotate_node
    toks = xpath.tokens(s.arg)
    toks = drop_all_ws(toks)
    # expect a statement name
    if toks[0][0] != 'name':
        err_add(ctx.errors, s.pos, 'SYNTAX_ERROR',
                "expected name of statement, got %s" % toks[0][1])
    stmt_name = toks[0][1]
    # if the next argument is not a predicate, it means that we'll annotate
    # the one and only statement of this type
    if len(toks) == 1 or toks[1][0] != '[':
        matches = node.search(stmt_name)
        if len(matches) == 0:
            err_add(ctx.errors, s.pos, 'TAILF_ANNOTATATION_ERROR',
                    "found no '%s' statements as children of %s %s at %s"
                    % (stmt_name, node.raw_keyword, node.arg, node.pos))
            return
        if len(matches) > 1:
            err_add(ctx.errors, s.pos, 'TAILF_ANNOTATATION_ERROR',
                    "found too many '%s' statements as children of %s %s at %s"
                    % (stmt_name, node.raw_keyword, node.arg, node.pos))
            return
        node = matches[0]
    elif len(toks) == 1:
        return
    else:
        toks = toks[1:]
        if toks[0][0] == '[':
            # skip the argument name and equality sign
            toks = toks[3:]
            stmt_arg = toks[0][1]
            if stmt_arg[0] == "'" or stmt_arg[0] == '"':
                stmt_arg = stmt_arg[1:-1]
            else:
                err_add(ctx.errors, s.pos, 'SYNTAX_ERROR',
                        "expected quoted name of statement, got %s" \
                            % toks[0][1])
                return

            match = node.search_one(stmt_name, stmt_arg)
            if match is None:
                err_add(ctx.errors, s.pos, 'TAILF_ANNOTATATION_ERROR',
                        "found no '%s %s' statements as children of %s %s at %s"
                        % (stmt_name, stmt_arg, node.raw_keyword,
                           node.arg, node.pos))
                return
            else:
                node = match
        else:
            # cannot end up here
            return


    s.i_annotate_node = node
    for substmt in s.substmts:
        if substmt.keyword == (tailf, 'annotate-statement'):
            v_annotate_statement(ctx, substmt)
        else:
            substmt.parent = node
            if substmt.keyword in ['pattern']:
                node.i_is_derived = True
            if substmt.keyword != 'must':
                old = node.search_one(substmt.keyword)
                if old is not None:
                    node.substmts.remove(old)
            node.substmts.append(substmt)

def drop_all_ws(toks):
    res = []
    for tok in toks:
        if tok[0] != 'whitespace':
            res.append(tok)
    return res

def v_annotate(ctx, s):
    # called for each module loaded after expansion
    # check if the module is annotated
    for f in ctx.tailf_ann_mods:
        (m, _validated) = ctx.tailf_ann_mods[f]
        for a in m.search((tailf, 'annotate')):
            mod = find_target_module(ctx, a)
            if s == mod:
                # ok, this tailf:annotate statement is for our module,
                # apply it
                apply_annotation(ctx, a)

def apply_annotation(ctx, s):
    if s.arg == '*':
        # special case - apply annotation to all children
        if (hasattr(s.parent, 'i_annotate_node') and
            hasattr(s.parent.i_annotate_node, 'i_children')):
            nodes = s.parent.i_annotate_node.i_children
        else:
            err_add(ctx.errors, s.pos, 'TAILF_BAD_ANNOTATE', ())
            return None
    else:
        node = find_target_node(ctx, s)
        if node is not None:
            nodes = [node]
        else:
            nodes = []
    for node in nodes:
        for substmt in s.substmts:
            if substmt.keyword != (tailf,'annotate'):
                substmt.parent = node
                node.substmts.append(substmt)
            else:
                apply_annotation(ctx, substmt)

def v_chk_annotate(ctx, s):
    # only check the path to make sure it is correct - the apply
    # is done in v_annotate().
    if s.arg == '*':
        # special case - apply annotation to all children
        if (hasattr(s.parent, 'i_annotate_node') and
            hasattr(s.parent.i_annotate_node, 'i_children')):
            nodes = s.parent.i_annotate_node.i_children
        else:
            err_add(ctx.errors, s.pos, 'TAILF_BAD_ANNOTATE', ())
            return None
    else:
        find_target_node(ctx, s)


def find_target_module(ctx, stmt):
    if stmt.arg.startswith('/'):
        arg = stmt.arg
    else:
        arg = "/" + stmt.arg
    # parse the path into a list of two-tuples of (prefix,identifier)
    path = [(m[1], m[2]) for m in syntax.re_schema_node_id_part.findall(arg)]
    # find the module of the last node in the path
    (prefix, identifier) = path[-1]
    no_errors = []
    return statements.prefix_to_module(stmt.i_module, prefix,
                                       stmt.pos, no_errors)

def find_target_node(ctx, stmt):
    if stmt.arg.startswith('/'):
        is_absolute = True
        arg = stmt.arg
    else:
        is_absolute = False
        arg = "/" + stmt.arg
    # parse the path into a list of two-tuples of (prefix,identifier)
    path = [(m[1], m[2]) for m in syntax.re_schema_node_id_part.findall(arg)]
    # find the module of the first node in the path
    (prefix, identifier) = path[0]
    module = statements.prefix_to_module(stmt.i_module, prefix,
                                         stmt.pos, ctx.errors)
    if module is None:
        # error is reported by prefix_to_module
        return None
    if is_absolute:
        # find the first node
        node = statements.search_data_keyword_child(module.i_children,
                                                    module.i_modulename,
                                                    identifier)
        if node is None:
            # check all our submodules
            for inc in module.search('include'):
                submod = ctx.get_module(inc.arg)
                if submod is not None:
                    node = statements.search_data_keyword_child(
                        submod.i_children,
                        submod.i_modulename,
                        identifier)
                    if node is not None:
                        break
            if node is None:
                err_add(ctx.errors, stmt.pos, 'NODE_NOT_FOUND',
                        (module.arg, identifier))
                return None
        path = path[1:]
    else:
        if hasattr(stmt.parent, 'i_annotate_node'):
            node = stmt.parent.i_annotate_node
        else:
            err_add(ctx.errors, stmt.pos, 'TAILF_BAD_ANNOTATE', ())
            return None

    # then recurse down the path
    for (prefix, identifier) in path:
        module = statements.prefix_to_module(stmt.i_module, prefix, stmt.pos,
                                             ctx.errors)
        if module is None:
            return None
        child = None
        if hasattr(node, 'i_children'):
            child = statements.search_data_keyword_child(node.i_children,
                                                         module.i_modulename,
                                                         identifier)
            if child is None:
                if hasattr(node, 'i_not_supported'):
                    child = statements.search_data_keyword_child(
                        node.i_not_supported,
                        module.i_modulename,
                        identifier)
        if child is None:
            err_add(ctx.errors, stmt.pos, 'NODE_NOT_FOUND',
                    (module.arg, identifier))
            return None
        node = child

    stmt.i_annotate_node = node
    return node

# Expand tailf:symlink
# Inline types from tailf-common (NYI)
# Remove anything marked as tailf:hidden "full"
# Optionally keep tailf:action
# Optionally keep tailf:info
# Remove all other tailf: statements.
# Remove the import of tailf-common, if all refereneces to it has
#   been removed.

# FIXME: keep comments!!
def sanitize(ctx, m):
    keep_import = sanitize_tree(ctx, m, m)
    if not keep_import:
        # remove the import of tailf-common,
        i = m.search_one('import', 'tailf-common')
        if i is not None:
            idx = m.substmts.index(i)
            del m.substmts[idx]

_body_stmts = ['container', 'list', 'leaf', 'leaf-list',
               'augment',
               'rpc', 'notification']

def sanitize_tree(ctx, module, stmt, keep_import=False):
    def inline_type(ref, type_):
        t_arg = tr(ref.search_one('type'), no_default=False)
        type_.arg = t_arg
        # remove the path
        path = type_.search_one('path')
        type_.substmts.remove(path)
        # copy referenced substatements
        for ss in ref.search_one('type').substmts:
            type_.substmts.append(ss)

    def sanitize_node(s, keep_import):
        if s.keyword in ('leaf', 'leaflist', 'typedef'):
            type_ = s.search_one('type')
            if type_ is not None:
                if (type_.i_typedef is not None
                    and type_.i_typedef.i_module.arg == 'tailf-common'):
                    if ctx.opts.tailf_keep_typedefs:
                        keep_import = True
                    else:
                        type_.arg = type_.i_typedef.arg
                        if module.search_one('typedef',
                                             type_.i_typedef.arg) is None:
                            copy_typedef(module, type_.i_typedef)
                            keep_import = False
                elif type_.arg == 'leafref' and s.i_leafref_ptr is not None:
                    (ref, _pos) = s.i_leafref_ptr
                    if is_hidden_full(ref):
                        inline_type(ref, type_)

                elif type_.arg == 'leafref':
                    path_type_spec = s.i_leafref
                    x = statements.validate_leafref_path(
                        ctx, stmt,
                        path_type_spec.path_spec,
                        path_type_spec.path_)
                    if x is not None:
                        (ref, _1, _2) = x
                        if is_hidden_full(ref):
                            inline_type(ref, type_)

        keep_import = sanitize_tree(ctx, module, s, keep_import)
        return keep_import

    def tr(stmt, no_default=True):
        if hasattr(stmt, 'tailf_tr'):
            return stmt.tailf_tr
        else:
            stmt.tailf_tr = True
            # defprefix is the prefix to use for module, i.e., the
            # path doesn't have a prefix, and it is defined in module.
            # defprefix is only used if no_default is False.
            i_orig_module = stmt.i_orig_module
            defprefix = i_orig_module.i_prefix
            res = translate_prefixes(stmt,
                                     i_orig_module.i_prefixes,
                                     i_orig_module.i_prefix,
                                     i_orig_module,
                                     module.i_prefixes,
                                     defprefix,
                                     module,
                                     no_default)
            stmt.tailf_tr = res
            return res

    def patch_node(stmt, symlink_target):

        def patch_prefixes(stmt):
            remove = []
            for ch in stmt.substmts:
                if ch.keyword == 'when' and not ctx.opts.tailf_keep_when:
                    remove.append(ch)
                elif ch.keyword == 'must' and not ctx.opts.tailf_keep_must:
                    remove.append(ch)
                elif ch.keyword == 'path':
                    (ref, _pos) = ch.parent.parent.i_leafref_ptr
                    if (is_child(ref, symlink_target) and
                        not is_hidden_full(ref)):
                        ch.arg = tr(ch)
                    else:
                        # inline the type
                        t_arg = tr(ref.search_one('type'), no_default=False)
                        ch.parent.arg = t_arg
                        # remove the path
                        remove.append(ch)
                elif ch.keyword == 'type':
                    if ch.i_typedef is not None:
                        # figure out in which module this type is defined
                        i = ch.arg.find(':')
                        if i != -1:
                            prefix = ch.arg[:i]
                            typename = ch.arg[i+1:]
                        else:
                            prefix = ""
                            typename = ch.arg
                        ch.arg = tr(ch, no_default=False)
                patch_prefixes(ch)
            for x in remove:
                stmt.substmts.remove(x)

        # copy nodes from i_children into substmts, if they are not
        # already present
        if hasattr(stmt, 'i_children'):
            i = 0
            for ch in stmt.i_children:
                patch_node(ch, symlink_target)
                if stmt.search_one(ch.keyword, ch.arg) is None:
                    stmt.substmts.insert(i, ch)
                    i += 1
                i += 1
        patch_prefixes(stmt)

    # copy the list so that we can delete from the original
    list_ = []
    for s in stmt.substmts:
        list_.append(s)

    for s in list_:
        if util.is_prefixed(s.keyword) and s.keyword[0] == 'tailf-common':
            if s.keyword[1] == 'symlink' and not ctx.opts.tailf_remove_symlink:
                ptr = s.search_one((tailf, 'path')).tailf_target_node
                if ptr is None:
                    # error in symlink; we just remove it
                    stmt.substmts.remove(s)
                else:
                    # copy the target.  we also need to make sure
                    # all augmented nodes are present in the substms
                    # list.  further, we must fix prefixes and possibly
                    # add new imports to our module
                    new = ptr.copy()
                    if ((hasattr(stmt, 'i_config') and
                         stmt.i_config == True) and
                            ptr.i_config == False and
                            new.search_one('config') is None):
                        config_stmt = None
                        pptr = ptr;
                        while (config_stmt is None):
                            config_stmt = pptr.search_one('config')
                            pptr = pptr.parent
                        new.substmts.insert(0,config_stmt.copy())
                    new.arg = s.arg
                    patch_node(new, ptr)
                    keep_import = sanitize_node(new, keep_import)
                    # replace the symlink statement with the target
                    idx = stmt.substmts.index(s)
                    stmt.substmts[idx] = new
            elif (s.keyword[1] in ['info', 'info-html'] and
                  ctx.opts.tailf_keep_info):
                m = module.search_one('import', 'tailf-common')
                tailf_prefix = m.search_one('prefix').arg
                s.raw_keyword = (tailf_prefix, s.keyword[1])
                keep_import = True
            elif (s.keyword[1] in ['display-when'] and
                  ctx.opts.tailf_keep_display_when):
                m = module.search_one('import', 'tailf-common')
                tailf_prefix = m.search_one('prefix').arg
                s.raw_keyword = (tailf_prefix, s.keyword[1])
                keep_import = True
            elif s.keyword[1] == 'action' and ctx.opts.tailf_keep_actions:
                m = module.search_one('import', 'tailf-common')
                tailf_prefix = m.search_one('prefix').arg
                s.raw_keyword = (tailf_prefix, s.keyword[1])
                sanitize_node(s, keep_import)
                keep_import = True
            elif s.keyword[1] == 'dependency' \
            and ctx.opts.tailf_keep_dependency:
                m = module.search_one('import', 'tailf-common')
                tailf_prefix = m.search_one('prefix').arg
                s.raw_keyword = (tailf_prefix, s.keyword[1])
                sanitize_node(s, keep_import)
                keep_import = True
            else:
                stmt.substmts.remove(s)
        elif s.search_one((tailf, 'hidden'), 'full') is not None:
            stmt.substmts.remove(s)
        elif (ctx.opts.tailf_remove_body and
              s.parent == module and s.keyword in _body_stmts):
            stmt.substmts.remove(s)
        elif (ctx.opts.tailf_keep_non_std_xpath != True and
              s.keyword in ['must', 'when']):
            remove = False
            toks = xpath.tokens(s.arg)
            for (tokname, x) in toks:
                if (tokname == 'function' and
                    (x not in xpath.core_functions and
                     x not in statements.yang_xpath_functions)):
                    remove = True
            if remove:
                stmt.substmts.remove(s)
            else:
                keep_import = sanitize_node(s, keep_import)
        else:
            keep_import = sanitize_node(s, keep_import)
    return keep_import

def copy_typedef(module, typedef):
    module.substmts.append(typedef)

def gen_new_import(module, tomodname, revision):
    i = 0
    pre = "p" + str(i)
    while pre in module.i_prefixes:
            i = i + 1
            pre = "p" + str(i)
    module.i_prefixes[pre] = (tomodname, revision)
    imp = statements.Statement(module, module, None, 'import', tomodname)
    prefix = statements.Statement(module, imp, None, 'prefix', pre)
    imp.substmts.append(prefix)
    if revision is not None:
        rev = statements.Statement(module, imp, None, 'revision-date',
                                   revision)
        imp.substmts.append(rev)
    # we know that there is at least one import (for the symlink target)
    first_import = module.search_one('import')
    idx = module.substmts.index(first_import)
    module.substmts.insert(idx, imp)
    return pre

def translate_prefixes(stmt, oldmap, oldmodprefix, oldmod,
                       newmap0, newmodprefix, newmod,
                       no_default):
    s = stmt.arg
    newmodname = newmod.arg
    # create reverse prefix map (module,revision) -> prefix
    newmap = {}
    for (k, v) in newmap0.items():
        newmap[v] = k

    if oldmod.keyword == 'module':
        oldmodname = oldmod.arg
    else:
        oldmodname = oldmod.i_including_modulename
    oldmodrevision = None # FIXME

    ourmodprefix = None
    def set_ourmod_prefix():
        if oldmod.arg != newmodname:
            # we are a module importing another module,
            # use our prefix for that module.
            if oldmod.keyword == 'module':
                if oldmap[oldmodprefix] not in newmap:
                    return None
                ourmodprefix = newmap[oldmap[oldmodprefix]]
            else:
                # find the module that this submodule belongs to
                # in our map of prefixes
                belongs_to = oldmod.search_one('belongs-to').arg
                for (p, (v, _r)) in newmap0.items():
                    if v == belongs_to:
                        ourmodprefix = p
        else:
            ourmodprefix = newmodprefix
        return ourmodprefix

    def change_prefix((tokname, s)):
        if tokname == 'name' or tokname == 'prefix-match':
            i = s.find(':')
            if i != -1:
                prefix = s[:i]
                rest = s[i:]
                if prefix == oldmodprefix:
                    if ourmodprefix is None:
                        newprefix = set_ourmod_prefix()
                        if newprefix is None:
                            oldk = (oldmodname, oldmodrevision)
                            if oldk not in newmap:
                                newprefix = gen_new_import(newmod, oldmodname,
                                                           oldmodrevision)
                                newmap[oldk] = newprefix
                            else:
                                newprefix = newmap[oldk]
                    else:
                        newprefix = ourmodprefix
                else:
                    if oldmap[prefix] in newmap:
                        newprefix = newmap[oldmap[prefix]]
                    else:
                        # add an import
                        (modname, revision) = oldmap[prefix]
                        newprefix = gen_new_import(newmod, modname, revision)
                return newprefix + rest
            elif no_default == True:
                # no prefix found, this means current module; don't add prefix
                return s
            elif stmt.keyword == 'type' and stmt.i_typedef is None:
                # builtin type
                return s
            else:
                # no prefix, this means a reference to oldmod
                # do we have an import of this module?
                oldk = (oldmodname, oldmodrevision)
                if oldk not in newmap:
                    newprefix = gen_new_import(newmod, oldmodname,
                                               oldmodrevision)
                    newmap[oldk] = newprefix
                return newmap[oldk] + ':' + s
        else:
            return s

    toks = xpath.tokens(s)
    ls = [change_prefix(tok) for tok in toks]
    return ''.join(ls)

def is_child(s, p):
    if s.parent is None:
        return False
    if s.parent == p:
        return True
    return is_child(s.parent, p)

def is_hidden_full(s):
    if s.search_one((tailf, 'hidden'), 'full') is not None:
        return True
    elif s.parent is not None:
        return is_hidden_full(s.parent)
    else:
        return False
