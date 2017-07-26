set shell=/bin/sh

if exists('g:vimpager')
  " I find this annoying
  let g:vimpager.passthrough = 0
else
  " Since vimpager uses ASCII color codes, we want to disable vim’s highlighting
  colorscheme Tomorrow-Night
  syntax on
endif

execute pathogen#infect()
set background=dark
filetype plugin indent on
set number
set shellcmdflag=-ic
set backspace=indent,eol,start
set mouse=a

" Indentation
set expandtab
set shiftwidth=2
set softtabstop=2
au BufRead,BufNewFile *.md set filetype=markdown
au FileType yaml setlocal autoindent

" use the proper non-bright colors for diff syntax highlighting
hi diffRemoved term=NONE ctermfg=001
hi diffAdded term=NONE ctermfg=002
