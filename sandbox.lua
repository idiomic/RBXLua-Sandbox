-- Please note when using this sandbox:
--   -  Code running outside the sandbox can be tricked into freeing code from the
--      sandbox. In particular, great care should be taken when giving sandboxed
--      code access indirectly to setfenv / getfenv. Secure versions are provided
--      inside the sandbox so their use is fine.
--   -  The security of this code has not been exhaustively researched. There may be
--      undiscovered issues with it. Please do not use for production code. These
--      sandbox may be useful for debugging and discovering script behavior, but
--      should not be a standalone means of security.

-- Localize global functions to prevent a changing environment
-- from messing with this code. This should allow multiple
-- instances of this code to run at once. (Don't do that...)
local setfenv = setfenv
local getfenv = getfenv
local setmetatable = setmetatable
local getmetatable = getmetatable
local type = type
local select = select
local tostring = tostring
local newproxy = newproxy
local print = print
local next = next

-- Unfortunately metamethods have different ways of invoking
-- the default metamethods of values. This table provides
-- metamethods which does that, so they can be easily wrapped.
local Capsule = {}
Capsule.__metatable = 'This debug metatable is locked.'
function Capsule:__index(k)         return self[k]   end
function Capsule:__newindex(k, v)   self[k] = v      end
function Capsule:__call(...)        self(...)        end
function Capsule:__concat(v)        return self .. v end
function Capsule:__unm()            return -self     end
function Capsule:__add(v)           return self + v  end
function Capsule:__sub(v)           return self - v  end
function Capsule:__mul(v)           return self * v  end
function Capsule:__div(v)           return self / v  end
function Capsule:__mod(v)           return self % v  end
function Capsule:__pow(v)           return self ^ v  end
function Capsule:__tostring()       return tostring(self) end
function Capsule:__eq(v)            return self == v end
function Capsule:__lt(v)            return self < v  end
function Capsule:__le(v)            return self <= v end
function Capsule:__len()            return #self     end
local CapsuleMT = {__index = Capsule}

-- We don't want to prevent interfaces and 'data' from being
-- garbage collected. While unwrapped 'data' shouldn't be present
-- in lua, interfaces will always be present in lua for the
-- lifetime of the data. So, we remove the data when the interfaces
-- are garbage collected, and allow interfaces to be collected.
local original = setmetatable({}, {__mode = 'k'})
local wrapper  = setmetatable({}, {__mode = 'v'})

-- unwrap may need to wrap functions to make sure sandboxed functions passed out
-- remain sandboxed. This is a predefinition to keep wrap local but defined after,
-- so that wrap can call unwrap when calling outside functions.
local wrap
local unwrap

local secureVersions = {
	-- As shown by ForbiddenJ, setfenv could be called with the return value of
	-- getfenv to escape the sandbox. The environment returned by getfenv was wrapped
	-- and then when passed to setfenv it was unwrapped. If the original version
	-- was an unsandboxed envionrment, then the sandboxed code could gain access
	-- to unsandboxed functions. To stop this, setfenv called from sandboxed code
	-- will always check if the environment being overwritten is sandboxed, and if
	-- so then it will set the envirionment to a sandboxed version.
	[setfenv] = function(target, newWrappedEnv)
		-- We want to hide the fact that we are calling setfenv so that the sandbox
		-- doesn't break scripts or provide unexpected behavior. Since 0 is the
		-- thread envirionment, we only add 1 if the target is 1 or higher. Also,
		-- calling setfenv or getfenv with a negative number results in an error,
		-- we'll let getfenv / setfenv return the error and be sure not to adjust
		-- the value in that case.
		if type(target) == 'number' and target > 0 then
			target = target + 2
		elseif target == wrapper[target] then
			target = original[target]
		end
	
		-- Check if the old envirionment was wrapped to determine if we need to
		-- wrap the new one. Also make sure that if an invalid level or target was\
		-- was given that the error shows it was from setfenv, not getfenv.
		local success, oldEnv = pcall(getfenv, target)
		local newEnv = newWrappedEnv
		if not success or oldEnv == wrapper[oldEnv] then
			-- It's a wrap, folks!
			newEnv = newWrappedEnv
		else
			newEnv = original[newWrappedEnv]
		end

		return wrap(setfenv(target, newEnv))
	end,

	-- We simply want to modify getfenv to hide the sandbox. This isn't for security
	-- purposes (they can't harm the sandbox even if they could edit its environment),
	-- however we would prefer to have code function the same inside and outside the
	-- sandbox.
	[getfenv] = function(target, newWrappedEnv)
		-- see comments in setfenv above for an explenation of these conditions
		if type(target) == 'number' and target > 0 then
			target = target + 1
		elseif target == wrapper[target] then
			target = original[target]
		end
	
		return wrap(getfenv(target))
	end,
}

-- Since RBXLua isn't multithreaded and wrap/unwrap cannot
-- yield, i and n are fine being passed as upvalues. This
-- allows a very simple implmentation of a function wrapper
-- without the use of tables and unpack, which does not
-- always preserve nil values.
local i, n = 1, 0

-- This function is called to make sure no wrappers get passed
-- outside the sandbox. The only data that leaves the sandbox is
-- through metamethod and function call arguments. These arguments
-- are always unwrapped.
function unwrap(...)
    if i > n then
        i = 1
        n = select('#', ...)

        -- make sure we handle n = 0 correctly, otherwise we can break
        -- vararg functions like 'print()' because they see the number of
        -- inputs, including nil ones.
        if n == 0 then
            return
        end
    end

    -- value may be nil, we should proceed with caution.
    local value = select(i, ...)
    if value then
        if type(value) == 'function' then
            local wrappedFunc = wrapper[value]
            if wrappedFunc then
                local originalFunc = original[wrappedFunc]
                if originalFunc == value then
                    -- sandbox has access to original function. This func
                    -- must have been defined inside of the sandboxed code
                    return wrappedFunc
                else
                    return originalFunc
                end
            else
                -- this function wasn't passed into the sandbox and so must
                -- have originated from inside. We need to wrap it when
                -- it leaves so that any arguments passed in are handled.
                wrappedFunc = function(...)
                    return unwrap(value(wrap(...)))
                end
                wrapper[wrappedFunc] = wrappedFunc
                wrapper[value] = wrappedFunc
                original[wrappedFunc] = value
                return wrappedFunc
            end
        elseif wrapper[value] then
            value = original[wrapper[value]]
        end
    end

    -- wrap the other values too
    i = i + 1
    if i <= n then
        return value, unwrap(...)
    else
        return value
    end
end

-- The return value of all function and metamethod calls is wrapped.
function wrap(...)
    if i > n then
        i = 1
        n = select('#', ...)

        -- make sure we handle n = 0 correctly, otherwise we can break
        -- vararg functions like 'print()' because they see the number of
        -- inputs, including nil ones.
        if n == 0 then
            return
        end
    end

    -- value may be nil, we should proceed with caution.
    local value = select(i, ...)
    if value then
        local wrapped = wrapper[value]

        if not wrapped then
            local vType = type(value)
            if vType == 'function' then
				if secureVersions[value] then
	                wrapped = secureVersions[value]
				else
                	local func = value -- value will be changed to the wrapped version soon.
	                wrapped = function(...)
	                    return wrap(func(unwrap(...)))
	                end
				end
            elseif vType == 'table' then
                wrapped = setmetatable({}, Capsule)
            elseif vType == 'userdata' then
				wrapped = newproxy(true)
				local mt = getmetatable(wrapped)
				for key, value in next, Capsule do
					mt[key] = value
				end
            else
                wrapped = value
            end

            wrapper[value] = wrapped -- Same data, same wrapper. Preserves equality tests.
            wrapper[wrapped] = wrapped -- Prevent encapsulating capsules.
            original[wrapped] = value -- store the original value to perform operations on and unwrap
        end

        value = wrapped
    end

    -- wrap the other values too
    i = i + 1
    if i <= n then
        return value, wrap(...)
    else
        return value
    end
end

-- Here, we ensure each metamethod is wrapped.
for key, metamethod in next, Capsule do
    Capsule[key] = wrap(metamethod)
end

-- mwhaha, now even calling rawset is sandboxed...
-- the only functions not sandboxed are those included in the
-- locked string metatable (i.e. 'myStringVar:sub(i, j)')
return function()
    return setfenv(2, wrap(getfenv(2)))
end
