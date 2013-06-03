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
