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
"
" Accepts a count as an optional second parameter which will be the window
" height for split or preview commands like 'stag', 'ptjump', etc., or the
" match index to jump to for 'tag'.
function! s:GoToErlangTag(cmd, ...)
    let orig_isk = &l:isk
    setl isk+=:,#,?
    let ident = expand('<cword>')
    let &l:isk = orig_isk

    " With no count arg or zero count, we can execute directly
    if a:0 == 0 || a:1 == 0
        execute a:cmd ident
    else
        let cnt = a:1

        if a:cmd ==# 'tag'
            execute cnt a:cmd ident
        elseif a:cmd =~# '^s'
            let cmd = 'split +' . a:cmd[1:] . '\ ' . ident
            execute cnt cmd
        elseif a:cmd =~# '^p'
            " :pedit has a cursor movement bug that we work around with a mark
            " http://bit.ly/1F59kWA  :-(  Affects YouCompleteMe users, for one.
            normal! m`

            let cmd = 'pedit +' . a:cmd[1:] . '\ ' . ident
            call s:ExecWithPreviewHeight(cmd, cnt)

            normal! ``
        endif
    endif
endfunction

" Because :pedit can't take height as count like :split does, ugh why.
function! s:ExecWithPreviewHeight(cmd, height)
    let orig_height = &previewheight
    let &previewheight = a:height

    try
        execute a:cmd
    catch /E426/  " tag not found
        pclose    " Spurious preview window is left open, close it.
        echohl WarningMsg | echom v:exception | echohl None
    finally
        let &previewheight = orig_height
    endtry
endfunction

function! VimErlangTagsDefineMappings()
    " Count is index of match to jump to, as with :tag
    nnoremap <buffer> <c-]>         :<C-U>call <SID>GoToErlangTag('tag', v:count1)<cr>
    nnoremap <buffer> g<LeftMouse>  :<C-U>call <SID>GoToErlangTag('tag', v:count1)<cr>
    nnoremap <buffer> <c-LeftMouse> :<C-U>call <SID>GoToErlangTag('tag', v:count1)<cr>

    nnoremap <buffer> g]            :<C-U>call <SID>GoToErlangTag('tselect')<cr>
    nnoremap <buffer> g<c-]>        :<C-U>call <SID>GoToErlangTag('tjump')<cr>

    " Count is window height for split and preview mappings
    nnoremap <buffer> <c-w><c-]>    :<C-U>call <SID>GoToErlangTag('stag',     v:count)<cr>
    nnoremap <buffer> <c-w>]        :<C-U>call <SID>GoToErlangTag('stag',     v:count)<cr>
    nnoremap <buffer> <c-w>g]       :<C-U>call <SID>GoToErlangTag('stselect', v:count)<cr>
    nnoremap <buffer> <c-w>g<c-]>   :<C-U>call <SID>GoToErlangTag('stjump',   v:count)<cr>
    nnoremap <buffer> <c-w>}        :<C-U>call <SID>GoToErlangTag('ptag',     v:count)<cr>
    nnoremap <buffer> <c-w>g}       :<C-U>call <SID>GoToErlangTag('ptjump',   v:count)<cr>
endfunction

" vim:set expandtab sw=4 ts=4:
