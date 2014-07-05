-- Copyright (c) 2014 Travis Cross <tc@traviscross.com>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

-- This library implements a JSON parser and printer.
-- See: http://json.org/

local json={}

-- parser

-- Throughout this section, parsers by convention return five values:
--   - the parsed value
--   - whether the value was accepted by the parser; that is, is the
--     value definitely of this kind?
--   - error message if a parsing error occurred or nil; set to non-nil
--     even for unaccepted values as a higher function might be
--     expecting a value of a particular type
--   - the string
--   - the position in the string at the end of the parse

-- Because the string and the position offset is threaded through all
-- calls, it can be thought of as a stream.

local char
function char(s,p)
  return string.sub(s,p,p)
end

local eat_whitespace
local expect_char
function expect_char(s,p,c)
  if char(s,p) == c then
    return c,true,nil,eat_whitespace(s,p+1)
  end
  return nil,true,"expected "..c.." at "..p,s,p
end

local pat_find
function pat_find(s,p,pat)
  local n=string.find(s,pat,p)
  if n then
    return n
  else
    return #s+1
  end
end

function eat_whitespace(s,p)
  return s,pat_find(s,p,"[^ \t\n]")
end

local parse_string
local parse_value

local parse_object
function parse_object(s,p)
  if char(s,p) ~= "{" then
    return nil,false,"not an object",s,p
  end
  s,p=eat_whitespace(s,p+1)
  local r={}
  if char(s,p) == "}" then
    return r,true,nil,eat_whitespace(s,p+1)
  end
  while true do
    local k,v,a,e,_
    k,a,e,s,p=parse_string(s,p)
    if e then return r,a,e,s,p end
    _,a,e,s,p = expect_char(s,p,":")
    if e then return r,a,e,s,p end
    v,a,e,s,p=parse_value(s,p)
    if e then return r,a,e,s,p end
    r[k]=v
    if char(s,p) == "}" then
      return r,true,nil,eat_whitespace(s,p+1)
    elseif char(s,p) == "," then
      s,p=eat_whitespace(s,p+1)
    else
      return nil,true,"expected a comma or object end at "..p,s,p
    end
  end
end

local parse_array
function parse_array(s,p)
  if char(s,p) ~= "[" then
    return nil,false,"not an array",s,p
  end
  s,p=eat_whitespace(s,p+1)
  local r={}
  if char(s,p) == "]" then
    return r,true,nil,eat_whitespace(s,p+1)
  end
  while true do
    local v,a,e
    v,a,e,s,p=parse_value(s,p)
    if e then return v,a,e,s,p end
    table.insert(r,v)
    if char(s,p) == "]" then
      return r,true,nil,eat_whitespace(s,p+1)
    elseif char(s,p) == "," then
      s,p=eat_whitespace(s,p+1)
    else
      return nil,true,"expected a comma or array end at "..p,s,p
    end
  end
end

local parse_symbol
local parse_number

function parse_value(s,p)
  local v,a,e
  v,a,e,s,p=parse_string(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_number(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_object(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_array(s,p)
  if a then return v,a,e,s,p end
  v,a,e,s,p=parse_symbol(s,p)
  if a then return v,a,e,s,p end
  return nil,false,"not a value",s,p
end

function parse_symbol(s,p)
  if not string.find(char(s,p),"[a-zA-Z]") then
    return nil,false,"not a symbol",s,p
  end
  if string.sub(s,p,p+3) == "true" then
    return true,true,nil,eat_whitespace(s,p+4)
  elseif string.sub(s,p,p+4) == "false" then
    return false,true,nil,eat_whitespace(s,p+5)
  elseif string.sub(s,p,p+3) == "null" then
    return nil,true,nil,eat_whitespace(s,p+4)
  end
  return nil,true,"invalid symbol at "..p,s,p
end

function parse_string(s,p)
  if string.sub(s,p,p) ~= "\"" then
    return nil,false,"not a string",s,p
  end
  p=p+1
  local i=p l=#s
  while true do
    if char(s,p) == "\\" then
      p=p+1
    elseif char(s,p) == "\"" then
      break
    elseif p >= l then
      return nil,true,"unterminated string starting at "..i-1,s,p
    end
    p=p+1
  end
  return string.sub(s,i,p-1),true,nil,eat_whitespace(s,p+1)
end

function parse_number(s,p)
  if not string.find(char(s,p),"[-0-9]",1)  then
    return nil,false,"not a number",s,p
  end
  local e=pat_find(s,p,"[^-+0-9.eE]")
  local v=tonumber(string.sub(s,p,e-1))
  if v then
    return v,true,nil,eat_whitespace(s,e)
  else
    return nil,true,"invalid number value at "..p,s,p
  end
end

function parse_json(s)
  if type(s) ~= "string" then return nil,"non-string input" end
  local v,a,e,s,p=parse_value(s,1)
  if e then return nil,e end
  return v,nil
end
json.parse = parse_json

-- printer

function json_from(x)
  local k=type(x)
  if k == "string" then
    return "\""..x.."\""
  elseif k == "table" then
    if x[1] then
      return json_from_array(x)
    else
      return json_from_object(x)
    end
  elseif k == "number" then
    return tostring(x)
  else
    return tostring(x)
  end
end
json.from = json_from

function json_from_array(l)
  if not l then return "" end
  local m
  for _,v in pairs(l) do
    if m then m=m.."," else m="" end
    m=m..json_from(v)
  end
  return "["..(m or "").."]"
end
json.from_array = json_from_array

function json_from_object(tab)
  if not tab then return "" end
  local m
  for k,v in pairs(tab) do
    if m then m=m.."," else m="" end
    m=m.."\""..tostring(k).."\":"..json_from(v)
  end
  return "{"..(m or "").."}"
end
json.from_object = json_from_object

return json
