autocmd FileType erlang call s:DefineTagMappings()

function! VimtagsErlangJump(index)
    let bufnr = bufnr('%')
    let iskeyword = getbufvar(bufnr, "&iskeyword")
    setlocal iskeyword+=:
    exec "normal! " . g:VimtagsErlangJumpDict[a:index]
    call setbufvar(bufnr, "&iskeyword", iskeyword)
endfunction

let g:VimtagsErlangJumpDict = {}

function! s:Wrap(index, keys, keys2)
    exec "noremap <buffer> " . a:keys . " :call VimtagsErlangJump('" .
       \ a:index . "')<cr>"
    let g:VimtagsErlangJumpDict[a:index] = a:keys2
endfunction

function! s:DefineTagMappings()
    call s:Wrap(1,"<c-]>","\<c-]>")
    call s:Wrap(2,"g<LeftMouse>","g\<LeftMouse>")
    call s:Wrap(3,"<c-LeftMouse>","\<c-LeftMouse>")
    call s:Wrap(4,"g]","g]")
    call s:Wrap(5,"g<c-]>","g\<c-]>")
endfunction
