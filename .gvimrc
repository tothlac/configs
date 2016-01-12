
set vb t_vb=                        " don't beep on errors and don't show a visual bell either
set guifont=Osaka\ 14         " a sensible font

:set cursorline
setlocal iskeyword+=:
setlocal comments=n:%
setlocal formatoptions=croql
setlocal textwidth=80
setlocal makeprg=/home/laszlototh/work/Middleware/erlang/projects/cbe/make.sh\ %
setlocal shiftwidth=4 tabstop=4

set nocompatible        " don't be bug-compatible with the original vi
set expandtab smarttab  " spaces instead of tabs
set shiftwidth=4        " one indentation level is 4 spaces
set tabstop=4           " one tab means 4 spaces
set incsearch           " incremental search
set mouse=a             " better mouse support
set vb t_vb=            " don't beep on errors and don't show a visual bell either
set shortmess+=I        " turning off the intro
set showcmd             " show commands as they are typed in
set noequalalways       " don't automatically resize windows
set backspace=2         " 'normal' backspace behaviour
set autoindent          " Copy indent from current line when starting a new line 
set history=500         " size of command line history
set whichwrap=<,>,[,]   " left/right arrows in beginning/end of line will jump to the other line
set iskeyword=@,48-57,_,192-255  " see :help iskeyword
set linebreak           " break lines at word boundaries
set suffixes+=.beam     " these files should have lower priority then the others when doing filename completion
set nojoinspaces        " don't put two spaces after period
set number              " show line numbers
set nobackup            " don't create backup files
set hlsearch

filetype plugin indent on  " turn on filetype plugins
syntax on                  " turn on syntax
colorscheme default         " use default colorschema

autocmd BufEnter * lcd %:p:h  " change directory to the current file's directory
set incsearch		" do incremental searching

function! ErlangGetFunDef(var)
  let l:tagName=expand(a:var)
  let l:searchcmd="/^\\<" . l:tagName . "\\>"
  exe l:searchcmd
  nohl
endfunction

command! -nargs=? -complete=tag ErlangGetFunDefComm :call ErlangGetFunDef(<f-args>)
map <silent> <buffer> <Leader>* :ErlangGetFunDefComm <cword><CR>

function! ErlangGetVarDef(var)
  call search('^[a-z]', 'b')
  let l:searchcmd='/' . expand(a:var) . ''
  exe l:searchcmd
endfunction

function! ErlangGetDef(var)
  let l:defName=expand(a:var)
  let l:firstChar=l:defName[0]
  if l:firstChar == tolower(l:firstChar)
      call ErlangGetFunDef(l:defName)
  else
      call ErlangGetVarDef(l:defName)
  endif
endfunction

command! -nargs=? -complete=tag ErlangGetDefComm :call ErlangGetDef(<f-args>)
map <silent> <buffer> gd :ErlangGetDefComm <cword><CR>

"To comment/uncomment
vmap <buffer> <Leader>c :s/^/%/<CR>:nohl<CR>
vmap <buffer> <Leader>u :s/^%//<CR>:nohl<CR>

function! ErlangGetCall(var)
  let l:tagName=expand(a:var)
  let l:searchcmd="/^handle_call({" . l:tagName
  exe l:searchcmd
  nohl
endfunction

command! -nargs=? -complete=tag ErlangGetCallComm :call ErlangGetCall(<f-args>)
map <buffer> <Leader>s :ErlangGetCallComm <cword><CR>

map <F2> ?^[a-z][A-Za-z0-9_]*(<CR>yw<C-O>$a<CR><ESC>0d$i    io:format("~p:<ESC>p a:~p ~p~n", [?MODULE, ?LINE, self()]),<ESC>
map <F3> lbye$a<CR><ESC>d$i    io:format("~p ~p <ESC>pa: '~p' ~n", [?MODULE, ?LINE, <ESC>pa]),<ESC>==

"To comment/uncomment
vmap <buffer> <Leader>c :s/^/%/<CR>:nohl<CR>
vmap <buffer> <Leader>u :s/^%//<CR>:nohl<CR>

function! ErlangGetFunDef(var)
  let l:tagName=expand(a:var)
  let l:searchcmd="/^\\<" . l:tagName . "\\>"
  exe l:searchcmd
  nohl
endfunction

command! -nargs=? -complete=tag ErlangGetFunDefComm :call ErlangGetFunDef(<f-args>)
map <silent> <buffer> <Leader>* :ErlangGetFunDefComm <cword><CR>

function! ErlangGetVarDef(var)
  call search('^[a-z]', 'b')
  let l:searchcmd='/' . expand(a:var) . ''
  exe l:searchcmd
endfunction

function! ErlangGetDef(var)
  let l:defName=expand(a:var)
  let l:firstChar=l:defName[0]
  if l:firstChar == tolower(l:firstChar)
      call ErlangGetFunDef(l:defName)
  else
      call ErlangGetVarDef(l:defName)
  endif
endfunction

command! -nargs=? -complete=tag ErlangGetDefComm :call ErlangGetDef(<f-args>)
map <silent> <buffer> gd :ErlangGetDefComm <cword><CR>

" toggle colored right border after 80 chars
set colorcolumn=81
let s:color_column_old = 0

set tags=tags;/

" Serch word under cursor in current dir
map <C-F> <esc>:Grep<CR>

if has("cscope")
    set csprg=~/bin/cscope
    set csto=0
    set cst
    set nocsverb
    " add any database in current directory
    if filereadable("cscope.out")
        cs add cscope.out
        " else add database pointed to by environment
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif
endif

"Note background set to dark in .vimrc
highlight Normal     guifg=darkgray guibg=lightyellow
"highlight Normal     guifg=lightblue guibg=darkgray

colorscheme solarized
set background=light

map _ :ls<CR>:b

" normal copy/paste
vmap <C-c> y<Esc>i
vmap <C-x> d<Esc>i
imap <C-v> <Esc>pi
imap <C-y> <Esc>ddi
map <C-z> <Esc>
imap <C-z> <Esc>ui

map <Down> j
map <Up> k
map <S-Down> <c-e>
map <S-Up> <c-y>

set lines=67 columns=197


" toggle colored right border after 80 chars
set colorcolumn=81
let s:color_column_old = 0

function! s:ToggleColorColumn()
    if s:color_column_old == 0
        let s:color_column_old = &colorcolumn
        windo let &colorcolumn = 0
    else
        windo let &colorcolumn=s:color_column_old
        let s:color_column_old = 0
    endif
endfunction

nnoremap <Leader>8 :call <SID>ToggleColorColumn()<cr>
