
set nocompatible

let s:dein_dir = expand('~/.vim/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

if !isdirectory(s:dein_repo_dir)
  execute '!git clone https://github.com/Shougo/dein.vim ' . s:dein_repo_dir
endif

execute 'set runtimepath^=' . s:dein_repo_dir

call dein#begin(s:dein_dir)
call dein#add('Shougo/dein.vim')
call dein#add('tomasr/molokai')
call dein#add('itchyny/lightline.vim')
call dein#add('bronson/vim-trailing-whitespace')
call dein#add('rhysd/accelerated-jk')
call dein#end()

if dein#check_install()
  call dein#install()
endif

filetype plugin indent on

syntax on
colorscheme molokai
highlight Normal ctermfg=none ctermbg=none
highlight Visual ctermfg=none ctermbg=240

let g:lightline = {
\   'colorscheme': 'default',
\   'component': {
\     'readonly': '%{&readonly?"⭤":""}'
\   },
\   'separator': { 'left': '⮀', 'right': '⮂' },
\   'subseparator': { 'left': '⮁', 'right': '⮃' }
\ }
set laststatus=2
set noshowmode

let g:extra_whitespace_ignored_filetypes = [ 'markdown' ]
autocmd BufWritePre * if ShouldMatchWhitespace() | execute 'FixWhitespace' | endif

nmap j <Plug>(accelerated_jk_gj)
nmap k <Plug>(accelerated_jk_gk)

set nobackup
set noswapfile
set noundofile
set viminfo=

set hidden
