" Copyright 2013 Csaba Hoch
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

autocmd FileType erlang call VimTagsErlangDefineMappings()

function! VimtagsErlangSelect()
    let orig_isk = &isk
    set isk+=:
    normal "_vaw
    let &isk = orig_isk
endfunction

function! VimTagsErlangDefineMappings()
    nnoremap <buffer> <c-]>         :call VimtagsErlangSelect()<cr><c-]>
    nnoremap <buffer> g<LeftMouse>  :call VimtagsErlangSelect()<cr>g<LeftMouse>
    nnoremap <buffer> <c-LeftMouse> :call VimtagsErlangSelect()<cr><c-LeftMouse>
    nnoremap <buffer> g]            :call VimtagsErlangSelect()<cr>g]
    nnoremap <buffer> g<c-]>        :call VimtagsErlangSelect()<cr>g<c-]>
endfunction
