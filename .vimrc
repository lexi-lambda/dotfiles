set shell=/bin/sh

colorscheme Tomorrow-Night

execute pathogen#infect()
syntax on
set background=dark
filetype plugin indent on
set number
set shellcmdflag=-ic
set backspace=indent,eol,start
set mouse=a

" Indentation
set expandtab
set shiftwidth=4
set softtabstop=4
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile *.mcmeta set filetype=javascript
au FileType sol color sol
au FileType lua setlocal sw=2 sts=2
au FileType yaml setlocal sw=2 sts=2
au FileType yaml setlocal autoindent

" this just seems to cause problems (especially with git diffs)
let vimpager_disable_ansiesc = 1
" I find this annoying
let vimpager_passthrough = 0

