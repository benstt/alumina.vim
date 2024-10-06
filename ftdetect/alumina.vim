" vint: -ProhibitAutocmdWithNoGroup

autocmd BufRead,BufNewFile *.alu call s:set_alumina_filetype()

function! s:set_alumina_filetype() abort
    if &filetype !=# 'alumina'
        set filetype=alumina
    endif
endfunction

" vim: set et sw=4 sts=4 ts=8:
