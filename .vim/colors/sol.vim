set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="sol"

hi Comment ctermfg=39
hi Constant ctermfg=5
hi Identifier ctermfg=41
hi Statement ctermfg=208
hi PreProc ctermfg=94
hi Type ctermfg=160
hi Special ctermfg=243
hi Modifier ctermfg=246
