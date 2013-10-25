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
    if exists("g:erlang_tags_ignore") && g:erlang_tags_ignore != ""
        let script_opts = " --ignore " . g:erlang_tags_ignore
    else
        let script_opts = ""
    endif

    let script_output = system(s:exec_script . script_opts)
    if !v:shell_error
        return 0
    else
        echoerr "vim-erlang-tags: " . script_output
    endif
endfunction

command! ErlangTags call VimErlangTags()

function! VimErlangTagsSelect()
    let orig_isk = &isk
    set isk+=:
    normal "_vaw
    let &isk = orig_isk
endfunction

function! VimErlangTagsDefineMappings()
    nnoremap <buffer> <c-]>         :call VimErlangTagsSelect()<cr><c-]>
    nnoremap <buffer> g<LeftMouse>  :call VimErlangTagsSelect()<cr>g<LeftMouse>
    nnoremap <buffer> <c-LeftMouse> :call VimErlangTagsSelect()<cr><c-LeftMouse>
    nnoremap <buffer> g]            :call VimErlangTagsSelect()<cr>g]
    nnoremap <buffer> g<c-]>        :call VimErlangTagsSelect()<cr>g<c-]>
endfunction
