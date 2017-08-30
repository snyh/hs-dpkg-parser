#!/home/snyh/deepin-codes/r-inspector/r-inspector -script

local json = require("json")
local io = require("io")

function CacheAndSaveTo(shash, name, fn)
   local cacheName = string.format("%s/result/%s/%s/%s", CONFIG:Cache(), shash, SCRIPT_HASH, name)
   local content = cacheStringTo(cacheName, fn)
   local f = io.open(name, "w+")
   f:write(content)
   f:close()
end

function ParseBinaryCache(bp)
   return {
      url = (string.format("${download_host}/%s", bp:Filename()));
      sha256 = (bp:SHA256());
   };
end

function GenerateBinary(archive)
   local  fn = function(acc, name, cf)
      local bp, err = cf:ToBinary()
      assert(err == nil, err)
      assert(bp ~= nil, "Invalid binary package " .. name)

      local info, err = bp:Depends(acc.__arch__, acc.__profile__)
      assert(err == nil, err)
      local deps = {}
      if info ~= nil then
         deps = info:SimpleDeps()
      end

      local provides, priority = cf:GetArray("provides", ","), cf:Get("priority")
      local v = {
         name = name;
         src = bp:Source();
         deb = ParseBinaryCache(bp);
      }
      if #deps > 0 then
         v["runtimeDepends"] = deps;
      end
      if #provides > 0 then
         v["provides"] = provides
      end
      if priority ~= "" then
         v["priority"] = priority
      end
      acc[name] = v;
      return acc
   end
   local ret = FoldlArchive(fn, {__arch__ = CONFIG:Arch(); __profile__ = "";}, archive)
   ret.__arch__, ret.__profile__ = nil, nil
   return ret

end


function getSrcFiles(dsc)
   local t = {
      name = dsc:Package() .. "_" .. dsc:Version() .. ".dsc";
      files = {};
   }
   for _, v in ipairs(dsc:Files("${download_host}")) do
      table.insert(t.files, {
                      url = v:Path();
                      size = v:Size();
                      sha256 = v:SHA256();
      })
   end
   return t
end

function GenerateSource(arch, source)
   local fn = function(acc, name, cf)
      local dsc, err = cf:ToSource()

      local info, err = dsc:BuildDepends(acc.__arch__, acc.__profile__)
      assert(err == nil, err)
      local deps = {}
      if info ~= nil then
         deps = info:SimpleDeps()
      end

      acc[name] = {
         name = name;
--         version = dsc:Version();
         buildDepends = deps;
         src = getSrcFiles(dsc)
      }
      return acc
   end
   local ret = FoldlArchive(fn, {__arch__ = arch; __profile__ = "";}, source)
   ret.__arch__, ret.__profile__ = nil, nil
   return ret
end


local suite = NewSuite("http://pools.corp.deepin.com/mips64el", "unstable", CONFIG:Cache())
local arch = "mips64el"
CacheAndSaveTo(suite:Hash(), "binary.json", function() return json.encode(GenerateBinary(suite:Archive(arch)))end)
CacheAndSaveTo(suite:Hash(), "source.json", function() return json.encode(GenerateSource(arch, suite:Archive("source")))end)
