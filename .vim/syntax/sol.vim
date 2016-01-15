" Vim syntax file
" Language: sol
" Maintainer: Alexis King
" Latest Revision: 12 December 2012

if exists("b:current_syntax")
  finish
endif

" Clusters
syn cluster solSymbol contains=solDelimiter,solModifier
syn cluster solDatatype contains=solToken,solNumber,solBoolean,solKeyword,solString
syn cluster solPlainList contains=solListData,solListExecutable
syn cluster solObjList contains=solObjListData,solObjListExecutable
syn cluster solList contains=@solPlainList,@solObjList
syn cluster solListContents contains=solComment,@solSymbol,@solDatatype,@solList

" Comments
syn match solComment /;.*/

" Datatypes
syn match solToken /\v\:[^[:space:]()\[\]{}]+/
syn match solNumber /\v\-?\d+(\.\d+)?/
syn keyword solBoolean true false
syn keyword solKeyword nil undefined
syn region solString start=/"/ skip=/\\\@<!\\"/ end=/"/

" Lists
" match all types of lists: (), [], @(), @[]
syn region solListData matchgroup=solDelimiter start=/\v\(/ end=/\v\)/ contains=@solListContents
syn region solListExecutable matchgroup=solDelimiter start=/\v\[/ end=/\v\]/ contains=@solListContents,solFunction
syn region solObjListExecutable matchgroup=solDelimiter start=/\v\@@<=\[/ end=/\v\]/ contains=@solListContents,solObject,solMethod
" give priority to object lists after a '@'
" syn match solDelimiter /\v\@\[/ nextgroup=solObjListExecutable

" Functions/Methods
" These crazy regexes are all based on the solFunction version.
" The solObject regex is a copy, but the solMethod regex is a small
" modification of the first one copied twice (to get the second element).
" Additionally, the solObject regex is placed after the solMethod regex
" for priority reasons.
syn match solFunction /\v(\[\s*\S{-})@<=[^[:space:][\]@]+[[:space:]\n\r\]]@=/ contained
syn match solMethod /\v((\[\s*\S{-})@<=[^[:space:][\]@]+[[:space:]\n\r\]]@=\s*\S{-})@<=[^[:space:][\]@]+[[:space:]\n\r\]]@=/ contained
syn match solObject /\v(\[\s*\S{-})@<=[^[:space:][\]@]+[[:space:]\n\r\]]@=/ contained

" Symbols
syn match solDelimiter /[{}]/
syn match solModifier /[@^#]/

let b:current_syntax = "sol"
hi def link solDelimiter        Special
hi def link solModifier         Modifier
hi def link solComment		Comment
hi def link solObject           Statement
hi def link solFunction		Function
hi def link solMethod           Function
hi def link solToken            Type
hi def link solNumber		Float
hi def link solBoolean		Boolean
hi def link solString		String
