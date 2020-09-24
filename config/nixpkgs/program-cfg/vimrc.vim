set nocompatible              " be iMproved, required

let s:this_file = expand('<sfile>')

if has('nvim')
   let g:vimDir = expand("~/.config/nvim")
else
   let g:vimDir = expand("~/.vim")
endif

""Touch relative to vim directory
function! Touch(name)
	let l:filepath = expand(g:vimDir . "/" . a:name)
	if has('unix')
		call system("touch " . l:filepath)
	else
		"create file on windows
		call system("copy NUL " . l:filepath)
	endif
	return
endfunction

if empty(glob(expand(g:vimDir . '/spell')))
	call mkdir(glob(expand(g:vimDir . '/spell')))
endif
let s:spfle = glob(expand(g:vimDir . '/spell/en.utf-8.add'))
if empty(s:spfle)
	call Touch(s:spfle)
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""Normal Vim Config""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

set updatetime=750
" highlight current line
augroup CursorLine
	au!
	au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	au WinLeave * setlocal nocursorline
augroup END
set hlsearch

"Remove error bells
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

"Remap Control-c to its default behavior AND turn off highlighting
nnoremap <silent><C-c> :noh <CR><silent><C-c>

nmap <silent><C-c> <esc>

"Emacs keybind
nnoremap <silent><C-g> <esc>

"turn off insert at the bottom of the screen
set noshowmode

" unmap annoyingly close to esc key help key
noremap <F1> <nop>

map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

let mapleader = "<space>"

" Turn on fancy ex mode tab completion
set wildmenu

set incsearch
set ignorecase
set smartcase

"Indent manual
set foldmethod=syntax

" Turn on syntax highlinting and line numbering
syntax on
set number

augroup vimrc
	autocmd VimResized * :wincmd = "automatically rebalance windows on resize
	" fix statusline
	autocmd VimEnter * set nolazyredraw lazyredraw
augroup END
" always show statusline
set laststatus=2

filetype plugin indent on

" set system clipboard to the un-named one
set clipboard=unnamedplus

" j and k listen to the current text wrapping in visual mode and normal mode
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

vnoremap ^s :<del><del><del><del><del>s/\%V/g<left><left>
" substitute only in selection (whole line is default) in visual mode
vnoremap s :s/\%V/g<left><left>

" remap Y to copy to end of line (as opposed to another way of doing yy)
nnoremap Y y$

" Delete keys by default going into blackhole register, with 'cut' rebound to m
noremap gm m
noremap m d
noremap mm dd
" delete line contents
noremap mM 0D
noremap M D
noremap dd "rdd
" delete line contents
noremap mM 0D
noremap dD 0"rD
noremap x "rx
noremap X "rX
noremap d "rd
noremap D "rD
noremap c "rc
noremap C "rC
noremap cc "rcc

"""Space remapping stuff
nnoremap <space>sa :%s//g<left><left>
nnoremap <space>sc :%s//gc<left><left><left>
nnoremap <space>bb :w<CR>
nnoremap <space>ww <C-w><C-w>
nnoremap <space>wl <C-w><C-l>
nnoremap <space>wh <C-w><C-h>
nnoremap <space>wj <C-w><C-j>
nnoremap <space>wk <C-w><C-k>

"""Persistant Undo
" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

if has('gui_running')
  set guioptions-=T            "turn off toolbar
  set guioptions-=m            "turn off menubar

  "turn off scrollbars
  set guioptions-=l
  set guioptions-=L
  set guioptions-=r
  set guioptions-=b
endif

set wrap linebreak nolist

"""make it easier to leave the damn terminal
if has('nvim')
    :tnoremap <A-h> <C-\><C-n><C-w>h
    :tnoremap <A-j> <C-\><C-n><C-w>j
    :tnoremap <A-k> <C-\><C-n><C-w>k
    :tnoremap <A-l> <C-\><C-n><C-w>l
endif

" Strip the newline from the end of a string
function! Chomp(str)
  return substitute(a:str, '\n$', '', '')
endfunction

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
  let fname = Chomp(system("git ls-files | dmenu -i -l 20 -p " . a:cmd))
  if empty(fname)
    return
  endif
  execute a:cmd . " " . fname
endfunction


"""Spellcheck
    " Creates a spellcheck mode, akin to word and the like
    " instead of having weird-ass bindings like [s to find
    " the next one and keeps annoying spellcheck highlighting
    " off your screen when you don't care about it
    function! MySpellCheck ()

    	if &spell
    		set spell!
    		unmap <buffer> n
    		unmap <buffer> N
    		unmap <buffer> s
    		unmap <buffer> a
    		unmap <buffer> y
    		nnoremap <buffer> y "+y
    		unmap <buffer> l
    	else
    		set spell
    		nnoremap <buffer> n ]s
    		nnoremap <buffer> N [s
    		" substitute current word
    		nnoremap <buffer> s z=1
    		"add to dictionary
    		nnoremap <buffer> a zg
    		"undo add
    		nnoremap <buffer> y zug
    		nnoremap <buffer> <silent> l :spellrepall <CR>
    	endif

    endfunction

    nnoremap <silent> <F7> :call MySpellCheck() <CR>
    set spelllang=en
    let spellfile=s:spfle



"""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""Plugins""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

"""Gundo
noremap <F9> :GundoToggle<CR>


if has("autocmd") && exists("+omnifunc")
	autocmd Filetype *
	    \	if &omnifunc == "" |
	    \		setlocal omnifunc=syntaxcomplete#Complete |
	    \	endif
endif
