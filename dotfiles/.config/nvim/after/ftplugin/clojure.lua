-- Clojure mappings
vim.keymap.set('n', '<localleader>e', 'cp', { remap = true, buffer = true })
vim.keymap.set('n', '<localleader>f', 'cpaF', { remap = true , buffer = true})
vim.keymap.set('n', '<localleader>k', ':%Eval<cr>', { buffer = true })

-- fireplace is using these mappings, so take them back
vim.api.nvim_create_autocmd('User', {
  pattern = 'FireplaceActivate',
  callback = function()
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { buffer = true })
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { buffer = true })
  end
})

local function thread_single_things(node, forms)
  if node:type() ~= 'list_lit' then
    table.insert(forms, vim.treesitter.get_node_text(node, 0))
    return vim.iter(forms):rev()
  elseif node:child_count() == 3 then
    table.insert(forms, vim.treesitter.get_node_text(node:child(1), 0))
    return vim.iter(forms):rev()
  end
end

local function child_texts(node)
  local strings = {}
  for c in node:iter_children() do
    if c:type() ~= '(' and c:type() ~= ')' then
      table.insert(strings, vim.treesitter.get_node_text(c, 0))
    end
  end

  return strings
end

local function thread_first_form(node, forms)
  local iter = thread_single_things(node, forms)
  if iter then return iter end

  local strings = child_texts(node)
  table.remove(strings, 2)
  if #strings == 1 then
    table.insert(forms, strings[1])
  else
    table.insert(forms, '(' .. table.concat(strings, ' ') .. ')')
  end

  return thread_first_form(node:child(2), forms)
end

local function thread_last_form(node, forms)
  local iter = thread_single_things(node, forms)
  if iter then return iter end

  local strings = child_texts(node)
  table.remove(strings)
  if #strings == 1 then
    table.insert(forms, strings[1])
  else
    table.insert(forms, '(' .. table.concat(strings, ' ') .. ')')
  end

  return thread_last_form(node:child(node:child_count() - 2), forms)
end

local function format_sexpr(start_row, start_col)
  vim.api.nvim_win_set_cursor(0, { start_row + 1, start_col })
  vim.api.nvim_feedkeys('=%', 'n', false)
end

local function inner_list()
  local node = vim.treesitter.get_node()
  while node and node:type() ~= "list_lit" do
    node = node:parent()
  end
  return node
end

local function thread_first()
  local node = inner_list()
  if node then
    local forms = thread_first_form(node, {}):totable()

    forms[1] = '(-> ' .. forms[1]
    forms[#forms] = forms[#forms] .. ')'

    local start_row, start_col, end_row, end_col = node:range()
    vim.api.nvim_buf_set_text(0,
      start_row, start_col, end_row, end_col,
      forms
    )

    format_sexpr(start_row, start_col)
  end
end

local function thread_last()
  local node = inner_list()
  if node then
    local forms = thread_last_form(node, {}):totable()

    forms[1] = '(->> ' .. forms[1]
    forms[#forms] = forms[#forms] .. ')'

    local start_row, start_col, end_row, end_col = node:range()
    vim.api.nvim_buf_set_text(0,
      start_row, start_col, end_row, end_col,
      forms
    )

    format_sexpr(start_row, start_col)
  end
end

vim.keymap.set('n', '<localleader>rl', thread_last, { buffer = true })
vim.keymap.set('n', '<localleader>rf', thread_first, { buffer = true })
