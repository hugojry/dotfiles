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

  'nvim-lua/plenary.nvim',
  'nvim-telescope/telescope.nvim',

  'nvim-treesitter/nvim-treesitter',

  'tpope/vim-repeat',
  'guns/vim-sexp',

  -- Clojure
  'tpope/vim-fireplace',
  'tpope/vim-dispatch',
  'radenling/vim-dispatch-neovim',
  'clojure-vim/vim-jack-in',
}

vim.o.hlsearch = true

vim.o.mouse = 'a'

vim.o.clipboard = 'unnamedplus'

vim.o.undofile = true

vim.o.ignorecase = true
vim.o.smartcase = true

vim.wo.signcolumn = 'no'

vim.o.updatetime = 250
vim.o.timeoutlen = 750

vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.hl.on_yank()
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

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>fr', require('telescope.builtin').oldfiles)
vim.keymap.set('n', '<localleader>bb', require('telescope.builtin').buffers)
vim.keymap.set('n', '<C-s>', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    previewer = false,
  })
end)

vim.keymap.set('n', '<leader>gf', require('telescope.builtin').git_files)
vim.keymap.set('n', '<leader>pf', function()
  if 1 == vim.fn.executable "rg" then
    require('telescope.builtin').find_files {
      find_command = { "rg", "--files", "--no-ignore", "--color", "never" }
    }
  else
    require('telescope.builtin').find_files {}
  end
end)
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags)
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string)
vim.keymap.set('n', '<leader>pg', require('telescope.builtin').live_grep)
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics)
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume)

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setqflist)
vim.keymap.set('n', '<leader>dl', vim.diagnostic.setloclist)
vim.keymap.set('n', ']w', ':lnext<cr>')
vim.keymap.set('n', '[w', ':lprevious<cr>')

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

local servers = {
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  pyright = {},
  clojure_lsp = {},
  vtsls = {},
  clangd = {},
  rust_analyzer = {},
}

for server_name, settings in pairs(servers) do
  local success, config = pcall(require, 'lsp.' .. server_name)
  if success then
    config.settings = settings
    vim.lsp.config[server_name] = config
    vim.lsp.enable(server_name)
  end
end

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('my.lsp', {}),
  callback = function(args)
    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))

    if client:supports_method('textDocument/completion') then
      vim.lsp.completion.enable(true, client.id, args.buf)
    end

    local nmap = function(keys, func)
      vim.keymap.set('n', keys, func, { buffer = true })
    end

    nmap('<leader>rn', vim.lsp.buf.rename)
    nmap('<leader>ca', vim.lsp.buf.code_action)

    nmap('gd', vim.lsp.buf.definition)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, { nowait = true })
    nmap('gR', require('telescope.builtin').lsp_references)
    nmap('gI', require('telescope.builtin').lsp_implementations)
    nmap('<leader>D', vim.lsp.buf.type_definition)
    nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols)
    nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols)

    nmap('<C-h>', vim.lsp.buf.hover)
    nmap('<C-k>', vim.lsp.buf.signature_help)

    -- Lesser used LSP functionality
    nmap('gD', vim.lsp.buf.declaration)
    nmap('<leader>wa', vim.lsp.buf.add_workspace_folder)
    nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder)
    nmap('<leader>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end)

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(args.buf, 'Format', function(_)
      vim.lsp.buf.format()
    end, {})
  end,
})

-- Setup neovim lua configuration
require('lazydev').setup()

vim.cmd([[packadd! matchit]])

vim.keymap.set('n', '<leader>ff', ':e %:h/<C-d>')
vim.keymap.set('n', '-', ':e %:h<cr>')
vim.keymap.set('n', '<C-j>', 'i<cr><esc>k:s/ \\+$//e<cr>j^')

local set_makeprg = function()
  vim.ui.input({ prompt = 'Compile command: ' }, function(input)
    if input and input ~= "" then
      vim.o.makeprg = input
    end
  end)
end

vim.keymap.set('n', '<localleader>c', ':make!<cr>')
vim.keymap.set('n', '<localleader>C', set_makeprg)

local bind_ex_command = function()
  vim.ui.input({ prompt = 'EX command: :' }, function(input)
    if input and input ~= "" then
      vim.keymap.set('n', '<leader>re', ':' .. input .. '<cr>')
    end
  end)
end

vim.keymap.set('n', '<leader>rE', bind_ex_command)
vim.keymap.set('n', '<localleader>bk', ':b# | bd#<cr>')
vim.keymap.set('n', '<localleader>qq', ":echo getqflist({'title': 1}).title<cr>")

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

  vim.g.sexp_insert_after_wrap = false
  buffer_map('n', '[r', '<Plug>(sexp_swap_element_backward)')
  buffer_map('n', ']r', '<Plug>(sexp_swap_element_forward)')
  buffer_map({ 'n', 'v' }, '<localleader>(', '<Plug>(sexp_round_head_wrap_element)')
  buffer_map({ 'n', 'v' }, '<localleader>[', '<Plug>(sexp_square_head_wrap_element)')
  buffer_map({ 'n', 'v' }, '<localleader>{', '<Plug>(sexp_curly_head_wrap_element)')

  local op = require('sexp.op')
  local motion = require('sexp.motion')

  buffer_map('n', '[s', op.slurp_barf_left, { expr = true })
  buffer_map('n', ']s', op.slurp_barf_right, { expr = true })
  buffer_map({ 'n', 'o', 'x' }, 'L', motion.forward_sexp)
  buffer_map({ 'n', 'o', 'x' }, 'H', motion.backward_sexp)
  buffer_map({ 'n', 'x', 'o' }, 'd', op.delete, { expr = true })
  buffer_map({ 'n', 'x', 'o' }, 'c', op.change, { expr = true })
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

local qf_group = vim.api.nvim_create_augroup('Qf', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'qf',
  group = qf_group,
  callback = function()
    vim.keymap.set('n', '<C-j>', function()
      pcall(function()
        vim.cmd('cnext')
        vim.api.nvim_feedkeys('zz', 'n', false)
        vim.cmd('wincmd p')
      end)
    end, { buffer = true })
    vim.keymap.set('n', '<C-k>', function()
      pcall(function()
        vim.cmd('cprev')
        vim.api.nvim_feedkeys('zz', 'n', false)
        vim.cmd('wincmd p')
      end)
    end, { buffer = true })
  end,
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
