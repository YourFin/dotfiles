set nocompatible              " be iMproved, required
filetype off                  " required

"""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""Neovim""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""
if has("win32") 
	let b:neoVimDir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
endif
function! ConfigNeovimSetup ()
	if has("unix")
		let s:uname = system("uname")
		let g:python_host_prog='/usr/bin/python'
		if s:uname == "Darwin\n"
			let g:python_host_prog='/usr/local/bin/python' 
		endif

		"deal with arch's python mapping to python3 stupidity
		if system('uname -a | grep -c ARCH')
			let g:python_host_prog='/usr/bin/python2'
		endif
		let g:vimDir =expand("~/.config/nvim")
	endif
	if has("win32")
		let g:vimDir = b:neoVimDir
	endif
	let s:editor_root=expand("~/.config/nvim")
	let &runtimepath.="," . g:vimDir . "/bundle/Vundle.vim"
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""normal vim""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""
function! ConfigVanillaVimSetup ()
	let s:editor_root=expand("~/.vim")
	let g:vimDir = "$HOME/.vim"
	set rtp+=~/.vim/bundle/Vundle.vim
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""universal setup"""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

if has('nvim')
    call ConfigNeovimSetup()
else
    call ConfigVanillaVimSetup()
endif
let s:this_file = expand('<sfile>')

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

let s:configured = 1
"create config folders and clone vundle if such things don't exist.
if empty(glob(expand(vimDir)))
	call mkdir(expand(vimDir))
endif
if empty(glob(expand(vimDir . '/bundle')))
	call mkdir((vimDir . '/bundle'))
	let s:configured = 0
endif
if empty(glob(expand(g:vimDir . '/spell')))
	call mkdir(glob(expand(g:vimDir . '/spell')))
endif
let s:spfle = glob(expand(g:vimDir . '/spell/en.utf-8.add'))
if empty(s:spfle)
	call Touch(s:spfle)
endif

"git clone automatically on unix
if empty(glob(expand(vimDir . '/bundle/Vundle.vim'))) && has("unix")
	call system(expand('git clone https://github.com/VundleVim/Vundle.vim.git ' . vimDir . '/bundle/Vundle.vim'))
endif

" set the runtime path to include Vundle and initialize
call vundle#rc(g:vimDir . '/bundle')
call vundle#begin(g:vimDir . '/bundle')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" plugin from http://vim-scripts.org/vim/scripts.html
Plugin 'L9'

"Visual
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'ctrlpvim/ctrlp.vim.git'
" only if ctags is installed
Plugin 'chrisbra/recover.vim'
Plugin 'flazz/vim-colorschemes'

" Only include in Full install
if ! empty(glob(expand(vimDir . '/full.conf')))
	Plugin 'valloric/youcompleteme'
	Plugin 'vim-syntastic/syntastic'
	"Individual Filetypes
	Plugin 'rust-lang/rust.vim'

endif

"Editing
Plugin 'tpope/vim-surround'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'terryma/vim-expand-region'
Plugin 'tpope/vim-sleuth'
Plugin 'sts10/vim-zipper'
Plugin 'vim-scripts/gitignore'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
"""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""End-Vundle"""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""Plugin Installation"""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

"""Full install
function! FullInstall()
	"Note that this touch function is further up my vimrc
	call Touch('full.conf')
	echom 'restart vim, and then run YcmCompile to finish'
	return	
endfunction
command InstallFull :call FullInstall()

function! CompileYcm()
	let l:command = 'python ' . glob(expand(g:vimDir . '/bundle/youcompleteme/install.py'))
	if has('unix')
		let l:command .= ' --clang-completer --omnisharp-completer'
	endif
	if system('hostname') =~# ".*firecakes.*"
		let l:command .= ' --system-libclang --racer-completer'
	endif
	echo system(command)
	return
endfunction
command YcmCompile :echo CompileYcm()

if ! s:configured
	PluginInstall
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
noremap dd "_dd
" delete line contents
noremap mM 0D
noremap dD 0"_D
noremap x "_x
noremap X "_X
noremap d "_d
noremap D "_D
noremap c "_c
noremap C "_C
noremap cc "_cc

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

"""Easymotion
    "Remap J & K to leader J K
    noremap <Leader>j J
    noremap <Leader>k K
    
    map <silent> L <Plug>(easymotion-lineforward)
    map <silent> J <Plug>(easymotion-j)
    map <silent> K <Plug>(easymotion-k)
    map <silent> H <Plug>(easymotion-linebackward)
    
    let g:EasyMotion_keys='aoeuihd,.k'



"""Multiple Cursors

" Have escape not leave multiple cursors unless in 'normal' mode
let g:multi_cursor_exit_from_visual_mode = 0
let g:multi_cursor_exit_from_insert_mode = 0


"""AirLine
let g:airline_skip_empty_sections = 1

"Laptop specific stuff
if system('hostname') =~# ".*firecakes.*"
	let g:airline_symbols = {}
	let g:airline#extensions#tabline#enabled = 1
	let g:airline_left_sep = ''
	let g:airline_left_alt_sep = ''
	let g:airline_right_sep = ''
	let g:airline_right_alt_sep = ''
	let g:airline_symbols.branch = ''
	let g:airline_symbols.readonly = ''
	let g:airline_symbols.linenr = ''
	let g:airline_detect_spell=1
	let g:airline#extensions#syntastic#enabled = 1
	let g:airline#extensions#tagbar#enabled = 0

	let g:airline_mode_map = {
	      \ '__' : '-',
	      \ 'n' : "N",
	      \ 'i'  : 'I',
	      \ 'R'  : 'R',
	      \ 'c'  : 'C',
	      \ 'v'  : 'V',
	      \ 'V'  : 'V',
	      \ '' : "VB",
	      \ 's'  : 'S',
	      \ 'S'  : 'S',
	      \ '' : 'S',
	      \ }
endif

"line items


"""syntastic 
let g:syntastic_check_on_open = 1

"""vimtex
let g:vimtex_view_method = 'mupdf'
let g:vimtex_use_temp_files = 1
let g:vimtex_view_general_viewer = 'mupdf'

if !exists('g:ycm_semantic_triggers')
	let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers.tex = [
			\ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
			\ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
			\ 're!\\hyperref\[[^]]*',
			\ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
			\ 're!\\(include(only)?|input){[^}]*',
			\ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
			\ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
			\ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
			\ ]


noh

if has("autocmd") && exists("+omnifunc")
	autocmd Filetype *
	    \	if &omnifunc == "" |
	    \		setlocal omnifunc=syntaxcomplete#Complete |
	    \	endif
endif
		    

"""colorscheme 
if system('hostname') =~# ".*firecakes.*"
	colorscheme desertink
	hi Search ctermbg=grey term=bold
else 
	colorscheme desertink
endif

"""Vim-expand-region
vmap v <plug>(expand_region_expand)
vmap V <plug>(expand_region_shrink)


"""Ctrl-P
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|zip|iso|img)$',
  \ 'link': '',
  \ }




"""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""Filetype Specific"""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""
"""Rust
autocmd FileType rust let g:syntastic_rust_checkers = ['rustc']
if ! system('hash rustc')
  let g:ycm_rust_src_path = system('rustc --print sysroot') . '/lib/rustlib/src/rust/src/'
endif
if &filetype =~ "rust"
	set iskeyword+=!
endif
