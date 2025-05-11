local sexp = require('sexp')
local pos_order = sexp.pos_order
local set_cur = sexp.set_cur
local get_cur = sexp.get_cur
local a = vim.api
local ts = vim.treesitter

local M = {}

local list_node_types = {
  list_lit = true,
  vec_lit  = true,
  map_lit  = true
}

local openers = {
  ["("] = ")",
  ["["] = "]",
  ["{"] = "}",
}

local closers = {
  [")"] = "(",
  ["]"] = "[",
  ["}"] = "{",
}

---@param start_row integer
---@param start_col integer
---@param end_row integer
---@param end_col integer
---@param dest_row integer
---@param dest_col integer
local function move_range(
  start_row, start_col, end_row, end_col, dest_row, dest_col
)
  local text = a.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {})
  -- dest starts after range starts
  if pos_order(start_row, start_col, dest_row, dest_col) > 0 then
    a.nvim_buf_set_text(0, dest_row, dest_col, dest_row, dest_col, text)
    if pos_order(end_row, end_col, dest_row, dest_col) > 0 then
      a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, {})
    else
      a.nvim_buf_set_text(0, start_row, start_col, dest_row, dest_col, {})
    end
  else
    a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, {})
    a.nvim_buf_set_text(0, dest_row, dest_col, dest_row, dest_col, text)
  end
end

---@param node TSNode
---@param count integer
local function slurp_left(node, count)
  local sib = node:prev_named_sibling()
  if not sib then return end
  for _ = 1, count - 1 do
    local sib_next = sib:prev_named_sibling()
    if not sib_next then
      break
    else
      sib = sib_next
    end
  end

  local row, col = node:range()
  local start_row, start_col = sib:range()

  assert(row)
  assert(col)
  assert(start_row)
  assert(start_col)

  move_range(row, col, row, col + 1, start_row, start_col)
  set_cur(start_row, start_col)
end

---@param node TSNode
---@param count integer
local function slurp_right(node, count)
  local sib = node:next_named_sibling()
  if not sib then return end
  for _ = 1, count - 1 do
    local sib_next = sib:next_named_sibling()
    if not sib_next then
      break
    else
      sib = sib_next
    end
  end

  local _, _, row, col = node:range()
  local _, _, end_row, end_col = sib:range()

  assert(row)
  assert(col)
  assert(end_row)
  assert(end_col)

  move_range(row, col - 1, row, col, end_row, end_col)
  set_cur(end_row, end_col - (row == end_row and 1 or 0))
end

---@param node TSNode
---@param count integer
local function barf_left(node, count)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local retained_child = node:named_child(
    math.max(0, named_child_count - 1 - count)
  )
  if not retained_child then return end

  local _, _, row, col = node:range()
  local _, _, end_row, end_col = retained_child:range()

  assert(row)
  assert(col)
  assert(end_row)
  assert(end_col)

  move_range(row, col - 1, row, col, end_row, end_col)
  set_cur(end_row, end_col)
end

---@param node TSNode
---@param count integer
local function barf_right(node, count)
  local named_child_count = node:named_child_count()
  if named_child_count < 2 then return end

  local retained_child = node:named_child(
    math.min(count, named_child_count - 1)
  )
  if not retained_child then return end

  local row, col = node:range()
  local start_row, start_col = retained_child:range()

  assert(row)
  assert(col)
  assert(start_row)
  assert(start_col)

  move_range(row, col, row, col + 1, start_row, start_col)
  set_cur(start_row, start_col - (row == start_row and 1 or 0))
end

---@param is_left boolean
---@param count integer
local function slurp_barf(is_left, count)
  local node = ts.get_node()
  if not node then return end

  node = sexp.find_node_types(list_node_types, node)
  if not node then return end

  local row, col = get_cur()

  local cur_text = a.nvim_buf_get_text(0, row, col, row, col + 1, {})[1]
  if openers[cur_text] then
    (is_left and slurp_left or barf_right)(node, count)
  elseif closers[cur_text] then
    (is_left and barf_left or slurp_right)(node, count)
  else
    (is_left and slurp_left or slurp_right)(node, count)
  end
end

function M.slurp_barf_left_op()
  slurp_barf(true, vim.v.count1)
end

function M.slurp_barf_right_op()
  slurp_barf(false, vim.v.count1)
end

---@param text string[]
---@return string[]
local function remaining_text(text)
  local stack = {}
  local unmatched_closers = {}
  local unmatched_openers = {}

  for _, line in ipairs(text) do
    for i = 1, #line do
      local char = string.sub(line, i, i)

      if closers[char] then
        local matching_opener = closers[char]
        if #stack > 0 and stack[#stack] == matching_opener then
          table.remove(stack)
        else
          -- This closer must match something before the range
          table.insert(unmatched_closers, char)
        end
      elseif openers[char] then
        table.insert(stack, char)
      end
    end
  end

  -- Any remaining openers must match closers after the range
  for i = #stack, 1, -1 do
    table.insert(unmatched_openers, stack[i])
  end

  local new_text

  if #text == 1 then
    -- Place closers at the beginning, openers at the end
    local closers_str = table.concat(unmatched_closers)
    local openers_str = table.concat(unmatched_openers)

    if closers_str ~= "" or openers_str ~= "" then
      new_text = {closers_str .. openers_str}
    else
      new_text = {}
    end
  else
    new_text = {}

    -- Add closers to the first line if there are any
    if #unmatched_closers > 0 then
      table.insert(new_text, table.concat(unmatched_closers))
    end

    -- Add openers to the last line if there are any
    if #unmatched_openers > 0 then
      table.insert(new_text, table.concat(unmatched_openers))
    end
  end

  return new_text
end

---@param type string
function M.delete_op(type)
  -- Deleting is inclusive
  local start_mark = a.nvim_buf_get_mark(0, "[")
  local end_mark = a.nvim_buf_get_mark(0, "]")

  local start_row = start_mark[1] - 1
  local end_row = end_mark[1] - 1

  local start_col, end_col
  if type == "line" then
    start_col = 0
    end_col = #a.nvim_buf_get_lines(0, end_row, end_row + 1, false)[1]
  else
    start_col = start_mark[2]
    end_col = end_mark[2]
  end

  if type == "char" or type == "block" then
    end_col = end_col + 1
  end

  local current_text = a.nvim_buf_get_text(0, start_row, start_col, end_row, end_col, {})
  local new_text = remaining_text(current_text)

  if type == "line" then
    a.nvim_buf_set_text(0, start_row, start_col, end_row, end_col, new_text)
    a.nvim_feedkeys('==', 'n', false)
  else
    a.nvim_buf_set_text(0, start_row, start_col, end_row + 1, 0, new_text)
  end
end

function M.slurp_barf_left()
  vim.go.operatorfunc = "v:lua.require'sexp.op'.slurp_barf_left_op"
  return "g@l"
end

function M.slurp_barf_right()
  vim.go.operatorfunc = "v:lua.require'sexp.op'.slurp_barf_right_op"
  return "g@l"
end

function M.delete()
  vim.go.operatorfunc = "v:lua.require'sexp.op'.delete_op"
  return "g@"
end

function M.change_op(type)
  M.delete_op(type)
  a.nvim_feedkeys('A', 'n', false)
end

function M.change()
  vim.go.operatorfunc = "v:lua.require'sexp.op'.change_op"
  return "g@"
end

return M
