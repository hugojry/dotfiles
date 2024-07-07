local ts = vim.treesitter
local M = {}

local function outer_list_node()
  local node = ts.get_node()
  while node and node:type() ~= "list_lit" do
    node = node:parent()
  end
  return node
end

local function inner_list_texts(node)
  local strings = {}
  for c in node:iter_children() do
    if c:type() ~= '(' and c:type() ~= ')' then
      table.insert(strings, ts.get_node_text(c, 0))
    end
  end

  return strings
end

local function expand_newlines(strings)
  local lines = {}
  for _, s in ipairs(strings) do
    for line in s:gmatch('[^\r\n]+') do
        table.insert(lines, line)
    end
  end
  return lines
end

local function thread(node, is_last)
  if not node then return end

  local form = node:child(2)
  if form:type() ~= 'list_lit' or form:child_count() == 3 then return end

  local texts = inner_list_texts(form)
  local next_form = is_last and table.remove(texts) or table.remove(texts, 2)
  local threaded_form = #texts == 1 and texts[1] or '(' .. table.concat(texts, ' ') .. ')'
  local lines = expand_newlines { next_form, threaded_form }
  local start_row, start_col, end_row, end_col = form:range()
  vim.api.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, lines)

  return true
end

function M.thread_all(is_last)
  local node = outer_list_node()
  if not node then return end

  local start_row, start_col, end_row, end_col = node:range()

  local thread_sym = is_last and '->>' or '->'
  vim.api.nvim_buf_set_text(0, end_row, end_col, end_row, end_col, { ')' })
  vim.api.nvim_buf_set_text(
    0, start_row, start_col, start_row, start_col, { '(' .. thread_sym .. ' ' }
  )

  local language_tree = ts.get_parser(0, 'clojure')

  language_tree:parse()
  node = ts.get_node({ pos = { start_row, start_col }})
  while thread(node, is_last) do
    language_tree:parse()
    node = ts.get_node({ pos = { start_row, start_col }})
  end

  vim.api.nvim_win_set_cursor(0, { start_row + 1, start_col })
  vim.api.nvim_feedkeys('=%', 'n', false)
end

M.thread_first = function() M.thread_all(false) end
M.thread_last = function() M.thread_all(true) end

local function assert_text(node)
  return ts.get_node_text(assert(node), 0)
end

function M.unwind()
  local node = outer_list_node()
  if not node then return end

  local thread_node = node:child(1)
  local thread_sym =  thread_node and ts.get_node_text(thread_node, 0)
  if thread_sym ~= '->' and thread_sym ~= '->>' then return end

  local source = node:child(2)
  local target = node:child(3)

  if not source or not target then return end

  local new_text
  if target:type() ~= 'list_lit' then
    new_text = '(' .. ts.get_node_text(target, 0) .. ' ' .. ts.get_node_text(source, 0) .. ')'
  else
    local components = { assert_text(target:child(1)) }
    local texts = inner_list_texts(target)
    if thread_sym == '->' then
      table.insert(components, ts.get_node_text(source, 0))
      table.remove(texts, 1)
      table.insert(components, table.concat(texts, ' '))
    else
      table.remove(texts)
      table.insert(components, table.concat(texts, ' '))
      table.insert(components, ts.get_node_text(source, 0))
    end
    new_text = '(' .. table.concat(components, ' ') .. ')'
  end

  local start_row, start_col = source:range()
  local _, _, end_row, end_col = target:range()
  vim.api.nvim_buf_set_text(
    0, start_row, start_col, end_row, end_col, expand_newlines { new_text }
  )

  vim.api.nvim_win_set_cursor(0, { start_row + 1, start_col })
  vim.api.nvim_feedkeys('=%', 'n', false)

  start_row, start_col = node:range()
  vim.api.nvim_win_set_cursor(0, { start_row + 1, start_col })
end

return M
