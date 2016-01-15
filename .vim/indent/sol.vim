" Vim indent file
" Language:	sol
" Maintainer:	Alexis King

if exists("b:did_indent")
	finish
endif
let b:did_indent = 1

if exists('*shiftwidth')
	let s:shiftwidth = function('shiftwidth')
else
	function s:shiftwidth()
		return &shiftwidth
	endfunction
endif

function! SolIndent(lnum)
	if a:lnum==1
		return 0
	endif
        let nonblank = prevnonblank(a:lnum-1)
	let line = substitute(substitute(getline(nonblank), '\v\"([^\"\\]|\\.)*\"', '', 'g'), ';.*', '', 'g')
	return indent(nonblank)+s:shiftwidth()*(len(substitute(line, '[^[({]', '', 'g'))-len(substitute(line, '^\s*[])}]\+\|[^])}]', '', 'g'))-len(substitute(substitute(getline(a:lnum), '^\s*', '', 'g'), '\(^[])}]*\).*', '\1', 'g')))
endfunction

setlocal indentexpr=SolIndent(v:lnum)
setlocal indentkeys=],),},o,O
let b:undo_indent='setlocal indentexpr<'
