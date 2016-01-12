set nocompatible        " don't be bug-compatible with the original vi
set expandtab smarttab  " spaces instead of tabs
set shiftwidth=4        " one indentation level is 4 spaces
set tabstop=4           " one tab means 8 spaces
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
set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:<

setlocal comments=n:%

filetype plugin indent on  " turn on filetype plugins
syntax on                 " turn on syntax


autocmd BufEnter * lcd %:p:h  " change directory to the current file's directory
set incsearch		" do incremental searching

"Note background set to dark in .vimrc
highlight Normal     guifg=gray guibg=lightblue


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

set tags=tags;/

let g:miniBufExplModSelTarget = 1
let g:miniBufExplorerMoreThanOne = 0
let g:miniBufExplModSelTarget = 0
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplVSplit = 25
let g:miniBufExplSplitBelow=1

map <c-m> :TMiniBufExplorer<cr>

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

" map <F2> ?^[a-z][A-Za-z0-9_]*(<CR>yw<C-O>$a<CR><ESC>0d$i    io:format("~p:<ESC>p a:~p ~p~n", [?MODULE, ?LINE, self()]),<ESC>
map <F2> :!~/.vim/compiler/erlang_check.erl %<CR>
map <F3> lbye$a<CR><ESC>d$i    io:format("~p ~p <ESC>pa: '~p' ~n", [?MODULE, ?LINE, <ESC>pa]),<ESC>==
map <F5> :Dash<CR>
map <F8> :!dialyze.escript %<CR>
map <F10> :!git diff %<CR>
map <F9> :!update_tags.erl %<CR>
map <F7> :Ack 
map _ :ls<CR>:b

nmap <C-S-n> : bnext<CR>
nmap <C-S-p> : bprev<CR>

"inoremap <C-[> <C-t>

map <C-n>  ]h
map <C-p>  [h

colorscheme delek
set nowrap

noremap <2-LeftMouse> :let @/ = expand('<cword>') <bar> set hls<cr>
imap <2-LeftMouse> <esc><2-LeftMouse>
"map <2-LeftMouse> *
"imap <2-LeftMouse> <c-o>*

set cul

set runtimepath^=/Users/laszlototh/mbin/vim-erlang-tags
set runtimepath^=/Users/laszlototh/mbin/ctrlp.vim

set nofoldenable

hi CursorLine   cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
hi CursorColumn cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white

autocmd BufWritePre *.erl :%s/\s\+$//e
autocmd BufWritePre *.hrl :%s/\s\+$//e
autocmd BufWritePre *.config :%s/\s\+$//e
autocmd BufWritePre *.args :%s/\s\+$//e
autocmd BufWritePre *.cpp :%s/\s\+$//e
autocmd BufWritePre *.c :%s/\s\+$//e

"erlang make for wombat

let s:wombat_options = '--outdir ../ebin --xref --load longnames vimerlang@127.0.0.1 wombat@127.0.0.1 --cookie wombat --copy /Users/laszlototh/w/wombat/wombat/rel'
let s:elibcloud_options = '--outdir ../ebin --xref'


"let g:erlang_flymake_options = ''
let g:erlang_make_options = '--outdir . --xref'
let g:erlang_make_options_rules =
  \ [{'path_re': 'wombat', 'options': s:wombat_options},
  \  {'path_re': '^/Users/laszlototh/Work/wombat', 'options': '--outdir ../ebin --xref'}]

let g:erlang_flymake_options_rules = [{'options': '--outdir ../ebin --xref'}]

set wildignore=*.beam,*.o,*/.eunit/*,*/rel/wombat/wombat/*

"To comment/uncomment
vmap <buffer> <Leader>k :s/^/%/<CR>:nohl<CR>
vmap <buffer> <Leader>u :s/^%//<CR>:nohl<CR>
