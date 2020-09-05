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

command! ErlangTags call vim_erlang_tags#VimErlangTags()

if exists("g:erlang_tags_auto_update") && g:erlang_tags_auto_update == 1
    au BufWritePost *.erl,*.hrl call vim_erlang_tags#AsyncVimErlangTags()
endif

if exists("g:erlang_tags_auto_update_current") && g:erlang_tags_auto_update_current == 1
    augroup vimErlangMaps
        au!
        autocmd BufWritePost *.erl,*.hrl call vim_erlang_tags#UpdateTags()
    augroup end
endif

function! VimErlangTagsDefineMappings()
    nnoremap <buffer> <c-]>         :call vim_erlang_tags#VimErlangTagsSelect(0)<cr><c-]>
    nnoremap <buffer> g<LeftMouse>  :call vim_erlang_tags#VimErlangTagsSelect(0)<cr><c-]>
    nnoremap <buffer> <c-LeftMouse> :call vim_erlang_tags#VimErlangTagsSelect(0)<cr><c-]>
    nnoremap <buffer> g]            :call vim_erlang_tags#VimErlangTagsSelect(0)<cr>g]
    nnoremap <buffer> g<c-]>        :call vim_erlang_tags#VimErlangTagsSelect(0)<cr>g<c-]>
    nnoremap <buffer> <c-w><c-]>    :call vim_erlang_tags#VimErlangTagsSelect(1)<cr><c-]>
    nnoremap <buffer> <c-w>]        :call vim_erlang_tags#VimErlangTagsSelect(1)<cr><c-]>
    nnoremap <buffer> <c-w>g<c-]>   :call vim_erlang_tags#VimErlangTagsSelect(1)<cr>g<c-]>
endfunction
