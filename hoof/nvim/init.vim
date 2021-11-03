call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'

Plug 'junegunn/limelight.vim'

Plug 'junegunn/goyo.vim'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'ryanoasis/vim-devicons'

Plug 'sheerun/vim-polyglot'

Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }

Plug 'pangloss/vim-javascript'

Plug 'ap/vim-css-color'

Plug 'vim-airline/vim-airline'

Plug 'vim-airline/vim-airline-themes'

Plug 'jiangmiao/auto-pairs'

Plug 'nvim-lua/plenary.nvim'

Plug 'nvim-telescope/telescope.nvim'

Plug 'arzg/vim-colors-xcode'

Plug 'tkztmk/vim-vala'

Plug 'ghifarit53/tokyonight-vim'

call plug#end()


autocmd!

set nocompatible
set number
syntax on
set fileencoding=utf-8
set encoding=utf-8
set title
set mouse=a
set autoindent
set background=dark
set nobackup
set hlsearch
set showcmd
set expandtab
set cmdheight=1
set laststatus=2
set scrolloff=10
set shell=bash

hi! Normal ctermbg=NONE guibg=NONE
"hi Foreground guibg=#ffffff

if has('nvim')
	set inccommand=split
endif

set nosc noru nosm

set lazyredraw
set ignorecase
set smarttab
set ai
set si
filetype plugin indent on
set shiftwidth=2
set tabstop=2
set nowrap
set path+=**
set wildignore+=*/node_modules/*
set cursorline
set termguicolors
set winblend=0
set wildoptions=pum
set pumblend=5
set clipboard=unnamedplus


let g:tokyonight_style = 'night' " available: night, storm
let g:tokyonight_enable_italic = 1
let g:tokyonight_transparent_background = 1
let g:airline_theme = "tokyonight"
let g:tokyonight_menu_selection_background = 'blue'
colorscheme tokyonight
"let g:neosolarized_termtrans=1

" =========================
" Custom Vim Functions
" =========================





"Goyo Settings
function! s:goyo_enter()
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
endfunction

function! s:goyo_leave()
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
  hi! Normal ctermbg=NONE guibg=NONE 
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()


"NERDTree setup

"Exit Vim if NERDTree is the only window left.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif

"Changing default NERDTree arrows
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

"KeyBind for NERDTree
"nnoremap <F4> :NERDTreeToggle<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :Telescope find_files<CR>
nnoremap <C-g> :Goyo<CR>


"KeyBind for TAGbar
nmap <F8> :TagbarToggle<CR>


let g:user_emmet_leader_key='<Tab>'
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:coc_global_extensions = [
  \ 'coc-tsserver',
  \ 'coc-json',
  \ 'coc-css',
  \  'coc-eslint',
  \  'coc-prettier',
  \  'coc-clangd'
  \ ]



autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear


nnoremap <silent> K :call CocAction('doHover')<CR>


"function! s:show_hover_doc()
"  call timer_start(500, 'ShowDocIfNoDiagnostic')
"endfunction


"autocmd CursorHoldI * :call <SID>show_hover_doc()
"autocmd CursorHold * :call <SID>show_hover_doc()

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gr <Plug>(coc-references)

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nnoremap <silent> <space>s :<C-u>CocList -I symbols<cr>

nnoremap <silent> <space>d :<C-u>CocList diagnostics<cr>

nmap <leader>do <Plug>(coc-codeaction)

nmap <leader>rn <Plug>(coc-rename)

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default'
