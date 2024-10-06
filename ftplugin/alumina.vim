" Language:     Alumina
" Description:  Vim ftplugin for Alumina
" Maintainer:   Benjamín García Roqués <benjamingarciaroques@gmail.com>
" Last Change:  October 10, 2024
" For bugs, patches and license go to https://github.com/benstt/alumina.vim

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

" vint: -ProhibitAbbreviationOption
let s:save_cpo = &cpo
set cpo&vim
" vint: +ProhibitAbbreviationOption

" Variables {{{1

" The alumina source code at present seems to typically omit a leader on /*!
" comments, so we'll use that as our default, but make it easy to switch.
" This does not affect indentation at all (I tested it with and without
" leader), merely whether a leader is inserted by default or not.
if get(g:, 'alumina_bang_comment_leader', 0)
    " Why is the `,s0:/*,mb:\ ,ex:*/` there, you ask? I don't understand why,
    " but without it, */ gets indented one space even if there were no
    " leaders. I'm fairly sure that's a Vim bug.
    setlocal comments=s1:/*,mb:*,ex:*/,s0:/*,mb:\ ,ex:*/,:///,://!,://
else
    setlocal comments=s0:/*!,ex:*/,s1:/*,mb:*,ex:*/,:///,://!,://
endif
setlocal commentstring=//%s
setlocal formatoptions-=t formatoptions+=croqnl
" j was only added in 7.3.541, so stop complaints about its nonexistence
silent! setlocal formatoptions+=j

" smartindent will be overridden by indentexpr if filetype indent is on, but
" otherwise it's better than nothing.
setlocal smartindent nocindent

if get(g:, 'alumina_recommended_style', 1)
    let b:alumina_set_style = 1
    setlocal shiftwidth=4 softtabstop=4 expandtab
    setlocal textwidth=99
endif

setlocal include=\\v^\\s*(pub\\s+)?use\\s+\\zs(\\f\|:)+
setlocal includeexpr=alumina#IncludeExpr(v:fname)

setlocal suffixesadd=.alu

if exists("g:ftplugin_alumina_source_path")
    let &l:path=g:ftplugin_alumina_source_path . ',' . &l:path
endif

if exists("g:loaded_delimitMate")
    if exists("b:delimitMate_excluded_regions")
        let b:alumina_original_delimitMate_excluded_regions = b:delimitMate_excluded_regions
    endif

    augroup alumina.vim.DelimitMate
        autocmd!

        autocmd User delimitMate_map   :call alumina#delimitmate#onMap()
        autocmd User delimitMate_unmap :call alumina#delimitmate#onUnmap()
    augroup END
endif

" Integration with auto-pairs (https://github.com/jiangmiao/auto-pairs)
if exists("g:AutoPairsLoaded") && !get(g:, 'alumina_keep_autopairs_default', 0)
    let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}
endif

if has("folding") && get(g:, 'alumina_fold', 0)
    let b:alumina_set_foldmethod=1
    setlocal foldmethod=syntax
    if g:alumina_fold == 2
        setlocal foldlevel<
    else
        setlocal foldlevel=99
    endif
endif

if has('conceal') && get(g:, 'alumina_conceal', 0)
    let b:alumina_set_conceallevel=1
    setlocal conceallevel=2
endif

" Motion Commands {{{1

" Bind motion commands to support hanging indents
nnoremap <silent> <buffer> [[ :call alumina#Jump('n', 'Back')<CR>
nnoremap <silent> <buffer> ]] :call alumina#Jump('n', 'Forward')<CR>
xnoremap <silent> <buffer> [[ :call alumina#Jump('v', 'Back')<CR>
xnoremap <silent> <buffer> ]] :call alumina#Jump('v', 'Forward')<CR>
onoremap <silent> <buffer> [[ :call alumina#Jump('o', 'Back')<CR>
onoremap <silent> <buffer> ]] :call alumina#Jump('o', 'Forward')<CR>

" Commands {{{1

" See |:AluminaRun| for docs
command! -nargs=* -complete=file -bang -buffer AluminaRun call alumina#Run(<bang>0, <q-args>)

" See |:AluminaExpand| for docs
command! -nargs=* -complete=customlist,alumina#CompleteExpand -bang -buffer AluminaExpand call alumina#Expand(<bang>0, <q-args>)

" See |:AluminaEmitIr| for docs
command! -nargs=* -buffer AluminaEmitIr call alumina#Emit("llvm-ir", <q-args>)

" See |:AluminaEmitAsm| for docs
command! -nargs=* -buffer AluminaEmitAsm call alumina#Emit("asm", <q-args>)

" See |:AluminaPlay| for docs
command! -range=% AluminaPlay :call alumina#Play(<count>, <line1>, <line2>, <f-args>)

" See |:AluminaFmt| for docs
command! -bar -buffer AluminaFmt call aluminafmt#Format()

" See |:AluminaFmtRange| for docs
command! -range -buffer AluminaFmtRange call aluminafmt#FormatRange(<line1>, <line2>)

" See |:AluminaInfo| for docs
command! -bar AluminaInfo call alumina#debugging#Info()

" See |:AluminaInfoToClipboard| for docs
command! -bar AluminaInfoToClipboard call alumina#debugging#InfoToClipboard()

" See |:AluminaInfoToFile| for docs
command! -bar -nargs=1 AluminaInfoToFile call alumina#debugging#InfoToFile(<f-args>)

" See |:AluminaTest| for docs
command! -buffer -nargs=* -count -bang AluminaTest call alumina#Test(<q-mods>, <count>, <bang>0, <q-args>)

if !exists("b:alumina_last_aluminac_args") || !exists("b:alumina_last_args")
    let b:alumina_last_aluminac_args = []
    let b:alumina_last_args = []
endif

" Cleanup {{{1

let b:undo_ftplugin = "
            \ setlocal formatoptions< comments< commentstring< include< includeexpr< suffixesadd<
            \|if exists('b:alumina_set_style')
                \|setlocal tabstop< shiftwidth< softtabstop< expandtab< textwidth<
                \|endif
                \|if exists('b:alumina_original_delimitMate_excluded_regions')
                    \|let b:delimitMate_excluded_regions = b:alumina_original_delimitMate_excluded_regions
                    \|unlet b:alumina_original_delimitMate_excluded_regions
                    \|else
                        \|unlet! b:delimitMate_excluded_regions
                        \|endif
                        \|if exists('b:alumina_set_foldmethod')
                            \|setlocal foldmethod< foldlevel<
                            \|unlet b:alumina_set_foldmethod
                            \|endif
                            \|if exists('b:alumina_set_conceallevel')
                                \|setlocal conceallevel<
                                \|unlet b:alumina_set_conceallevel
                                \|endif
                                \|unlet! b:alumina_last_aluminac_args b:alumina_last_args
                                \|delcommand AluminaRun
                                \|delcommand AluminaExpand
                                \|delcommand AluminaEmitIr
                                \|delcommand AluminaEmitAsm
                                \|delcommand AluminaPlay
                                \|nunmap <buffer> [[
                                \|nunmap <buffer> ]]
                                \|xunmap <buffer> [[
                                \|xunmap <buffer> ]]
                                \|ounmap <buffer> [[
                                \|ounmap <buffer> ]]
                                \|setlocal matchpairs-=<:>
                                \|unlet b:match_skip
                                \"

" }}}1

" Code formatting on save
augroup alumina.vim.PreWrite
    autocmd!
    autocmd BufWritePre *.alu silent! call aluminafmt#PreWrite()
augroup END

setlocal matchpairs+=<:>
" For matchit.vim (aluminaArrow stops `Fn() -> X` messing things up)
let b:match_skip = 's:comment\|string\|aluminaCharacter\|aluminaArrow'

" vint: -ProhibitAbbreviationOption
let &cpo = s:save_cpo
unlet s:save_cpo
" vint: +ProhibitAbbreviationOption

" vim: set et sw=4 sts=4 ts=8:
