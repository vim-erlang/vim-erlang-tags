" Copyright 2013 Csaba Hoch
" Copyright 2013 Adam Rutkowski
"
" Licensed under the Apache License, Version 2.0 (the "License");
" you may not use this file except in compliance with the License.
" You may obtain a copy of the License at
"
"     http://www.apache.org/licenses/LICENSE-2.0
"
" Unless required by applicable law or agreed to in writing, software
" distributed under the License is distributed on an "AS IS" BASIS,
" WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
" See the License for the specific language governing permissions and
" limitations under the License.

if exists("b:vim_erlang_tags_loaded")
    finish
else
    let b:vim_erlang_tags_loaded = 1
endif

autocmd FileType erlang call VimErlangTagsDefineMappings()

let s:exec_script = expand('<sfile>:p:h') . "/../bin/vim-erlang-tags.erl"

function! VimErlangTags()
    let script_opts = ""

    if exists("g:erlang_tags_ignore")
        let ignored_paths = (type(g:erlang_tags_ignore) == type("string") ?
                          \  [ g:erlang_tags_ignore ] :
                          \  g:erlang_tags_ignore)

        for path in ignored_paths
            let script_opts = script_opts . " --ignore " . path
        endfor
    endif

    if exists("g:erlang_tags_outfile") && g:erlang_tags_outfile != ""
        let script_opts = script_opts . " --output " . g:erlang_tags_outfile
    endif

    let script_output = system(s:exec_script . script_opts)
    if !v:shell_error
        return 0
    else
        echoerr "vim-erlang-tag " . script_output
    endif
endfunction

command! ErlangTags call VimErlangTags()

" Execute the given tag lookup for current word, where 'iskeyword' is
" temporarily set such that modules, records, and macros are included.
function! s:GoToErlangTag(cmd)
    let orig_isk = &isk
    set isk+=:,#,?
    let ident = expand('<cword>')
    let &isk = orig_isk

    execute a:cmd ident
endfunction

function! VimErlangTagsDefineMappings()
    nnoremap <buffer> <c-]>         :<C-U>call <SID>GoToErlangTag('tag')<cr>
    nnoremap <buffer> g<LeftMouse>  :<C-U>call <SID>GoToErlangTag('tag')<cr>
    nnoremap <buffer> <c-LeftMouse> :<C-U>call <SID>GoToErlangTag('tag')<cr>
    nnoremap <buffer> g]            :<C-U>call <SID>GoToErlangTag('tselect')<cr>
    nnoremap <buffer> g<c-]>        :<C-U>call <SID>GoToErlangTag('tjump')<cr>
    nnoremap <buffer> <c-w><c-]>    :<C-U>call <SID>GoToErlangTag('stag')<cr>
    nnoremap <buffer> <c-w>]        :<C-U>call <SID>GoToErlangTag('stag')<cr>
    nnoremap <buffer> <c-w>g]       :<C-U>call <SID>GoToErlangTag('stselect')<cr>
    nnoremap <buffer> <c-w>g<c-]>   :<C-U>call <SID>GoToErlangTag('stjump')<cr>
    nnoremap <buffer> <c-w>}        :<C-U>call <SID>GoToErlangTag('ptag')<cr>
    nnoremap <buffer> <c-w>g}       :<C-U>call <SID>GoToErlangTag('ptjump')<cr>
endfunction

" vim:set expandtab sw=4 ts=4:
