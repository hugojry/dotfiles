-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

local function clone_paq()
  local path = vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim"
  local is_installed = vim.fn.empty(vim.fn.glob(path)) == 0
  if not is_installed then
    vim.fn.system { "git", "clone", "--depth=1", "https://github.com/savq/paq-nvim.git", path }
    return true
  end
end

local function bootstrap_paq(packages)
  local first_install = clone_paq()
  vim.cmd.packadd("paq-nvim")
  local paq = require("paq")
  if first_install then
    vim.notify("Installing plugins... If prompted, hit Enter to continue.")
  end

  -- Read and install packages
  paq(packages)
  paq.install()
end

bootstrap_paq {
  "savq/paq-nvim",

  -- Git related plugins
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  -- Useful pairs of mappings using [ and ]
  'tpope/vim-unimpaired',

  'romainl/vim-cool',

  'j-hui/fidget.nvim',

  'folke/lazydev.nvim',

  'neovim/nvim-lspconfig',

  'nvim-lua/plenary.nvim',
  'nvim-telescope/telescope.nvim',

  'nvim-treesitter/nvim-treesitter',

  'mfussenegger/nvim-lint',

  'guns/vim-sexp',

  -- Clojure
  'tpope/vim-fireplace',
  'tpope/vim-dispatch',
  'radenling/vim-dispatch-neovim',
  'clojure-vim/vim-jack-in'
}

vim.o.hlsearch = true

vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

vim.wo.signcolumn = 'no'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 750

-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

require('fidget').setup({})

-- nvim-lint setup
require('lint').linters_by_ft = {
  clojure = {'clj-kondo'}
}
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  callback = function()
    require("lint").try_lint()
  end
})

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>fr', require('telescope.builtin').oldfiles, { desc = '[fr] Find recently opened files' })
vim.keymap.set('n', '<localleader>bb', require('telescope.builtin').buffers, { desc = '[,bb] Find existing buffers' })
vim.keymap.set('n', '<C-s>', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    previewer = false,
  })
end, { desc = '[<C-s>] Fuzzily search in current buffer' })

vim.keymap.set('n', '<leader>gf', require('telescope.builtin').git_files, { desc = 'Search [G]it [F]iles' })
vim.keymap.set('n', '<leader>pf', function()
  if 1 == vim.fn.executable "rg" then
    require('telescope.builtin').find_files {
      find_command = { "rg", "--files", "--no-ignore", "--color", "never" }
    }
  else
    require('telescope.builtin').find_files {}
  end
end, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>pg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume, { desc = '[S]earch [R]esume' })

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setqflist, { desc = 'Open diagnostics list' })
vim.keymap.set('n', '<leader>dl', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
      'c', 'lua', 'python', 'rust', 'vimdoc', 'vim', 'bash', 'clojure', 'dart', 'java',
      'json', 'xml'
    },

    -- Install languages synchronously (only applied to `ensure_installed`)
    sync_install = false,
    highlight = { enable = true },
    indent = { enable = true },
    modules = {},
    ignore_install = {},
    auto_install = false
  }
end, 0)

local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  nmap('<C-h>', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

local servers = {
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  pyright = {},
  clojure_lsp = {},
  vtsls = {}
}

for server_name, config in pairs(servers) do
  require('lspconfig')[server_name].setup {
    on_attach = on_attach,
    settings = config,
    filetypes = (servers[server_name] or {}).filetypes,
  }
end

-- Setup neovim lua configuration
require('lazydev').setup()


vim.cmd([[packadd! matchit]])

vim.keymap.set('n', '<leader>ff', ':e %:h/<C-d>')
vim.keymap.set('n', '-', ':e %:h<cr>')
vim.keymap.set('n', '<C-j>', 'i<cr><esc>k:s/ \\+$//e<cr>j^')

local set_makeprg = function()
  vim.ui.input({ prompt = 'Compile command: '}, function(input)
    if input and input ~= "" then
      vim.o.makeprg = input
    end
  end)
end

vim.keymap.set('n', '<localleader>c', ':make!<cr>')
vim.keymap.set('n', '<localleader>C', set_makeprg)

local bind_ex_command = function()
  vim.ui.input({ prompt = 'EX command: :'}, function(input)
    if input and input ~= "" then
      vim.keymap.set('n', '<leader>re', ':' .. input .. '<cr>')
    end
  end)
end

vim.keymap.set('n', '<leader>rE', bind_ex_command)
vim.keymap.set('n', '<localleader>bk', ':b# | bd#<cr>')

vim.api.nvim_create_user_command(
  'UnixLE',
  function() pcall(function() vim.cmd('%s/\r//') end) end,
  {}
)

vim.api.nvim_create_user_command(
  'DeleteTrailingWhitespace',
  function() pcall(function() vim.cmd('%s/ \\+$//') end) end,
  {}
)

local toggle_diagnostics = function()
  if vim.diagnostic.is_enabled() then
    vim.diagnostic.enable(false)
  else
    vim.diagnostic.enable(true)
  end
end

vim.keymap.set('n', '<localleader>dt', toggle_diagnostics)

local buffer_map = function(modes, lhs, rhs, opts)
  vim.keymap.set(modes, lhs, rhs, { buffer = true , expr = opts and opts.expr})
end

local setup_sexp_mappings = function()
  if not vim.g.sexp_loaded then
    return
  end

  local sexp = require('sexp')

  vim.g.sexp_insert_after_wrap = false
  buffer_map('n', '<localleader>r', '<Plug>(sexp_swap_element_forward)')
  buffer_map('n', '<localleader>R', '<Plug>(sexp_swap_element_backward)')
  buffer_map('n', '[s', sexp.slurp_barf_left, { expr = true })
  buffer_map('n', ']s', sexp.slurp_barf_right, { expr = true })
  buffer_map({ 'n', 'v' }, '<localleader>(', '<Plug>(sexp_round_head_wrap_element)')
  buffer_map({ 'n', 'v' }, '<localleader>[', '<Plug>(sexp_square_head_wrap_element)')
  buffer_map({ 'n', 'v' }, '<localleader>{', '<Plug>(sexp_curly_head_wrap_element)')
  buffer_map({ 'n', 'o', 'x' }, 'L', sexp.forward_sexp)
  buffer_map({ 'n', 'o', 'x' }, 'H', sexp.backward_sexp)
  buffer_map({ 'n', 'x', 'o' }, 'd', sexp.delete, { expr = true })
  buffer_map({ 'n', 'x', 'o' }, 'c', sexp.change, { expr = true })
end

local sexp_mappings_group = vim.api.nvim_create_augroup("sexp_mappings_for_hy", {})
vim.api.nvim_create_autocmd("FileType", {
  group = sexp_mappings_group,
  pattern = vim.g.sexp_filetypes or "lisp,scheme,clojure",
  callback = setup_sexp_mappings
})

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.bb',
  callback = function() vim.o.filetype = 'clojure' end
})

vim.g.netrw_banner = 0
vim.g.netrw_list_hide = '\\./,\\.\\./'

vim.o.termguicolors = true

vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4

vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
vim.o.foldlevel = 99

vim.o.splitright = true
vim.o.splitbelow = true

-- vim: ts=2 sts=2 sw=2 et
