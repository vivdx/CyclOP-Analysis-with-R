setOldClass("connection")
setOldClass(c("textConnection", "connection"))
setOldClass(c("file", "connection"))
setOldClass(c("pipe", "connection"))
setOldClass("AsIs")
setOldClass("Filename")
setOldClass("FileContent")
setOldClass("JSONParserHandler")
setOldClass("NativeSymbolInfo")
setOldClass("NativeSymbol")

isContent = 
  function(content)
  {
    inherits(content, "AsIs") || (!file.exists(content) && length(grep("^[[:space:]]*[[{]", content)))
  }

StrictLogical = 2L
StrictNumeric = 4L
StrictCharacter = 8L

Strict = StrictNumeric + StrictCharacter + StrictLogical

setGeneric("fromJSON",
           function(content, handler = NULL, default.size = 100, depth = 150L,
                    allowComments = TRUE,  asText = isContent(content),
                    data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict,  nullValue = NULL,
                    simplifyWithNames = TRUE, encoding = NA_character_, stringFun = NULL, ...)
             standardGeneric("fromJSON"))

setMethod("fromJSON", c("AsIs", handler = "NULL"),
          function(content,  handler = NULL, default.size = 100, depth = 150L,
                   allowComments = TRUE,  asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE,  encoding = NA_character_, stringFun = NULL, ...)
          {
            stringFunType = c("GARBAGE" = 4L)
            enc = mapEncoding(if(is.na(encoding)) Encoding(content) else encoding)
            if(!is.null(stringFun)) {
              if(!is.function(stringFun)) {
                stringFunType = getStringRoutineType(stringFun)
                if(is.character(stringFun))
                  stringFun = getNativeSymbolInfo(stringFun)
                if(is(stringFun, "NativeSymbolInfo"))
                  stringFun = stringFun$address
                else if( typeof(stringFun) != "externalptr")
                  stop("stringFun needs to be a function or identify a native routine")
              } else 
                stringFunType = c("R_FUNCTION" = 2L)
            }
            
            .Call("R_fromJSON", content, as.integer(sum(simplify)), nullValue, as.logical(simplifyWithNames), enc,
                  stringFun, stringFunType)  
          })

mapEncoding =
  function(encoding)
  {
    if(is.na(encoding))
      return(0L)
    
    codes = c(unknown = 0L, "native" = 0L, "utf8" = 1L,  "utf-8" = 1L, "latin1" = 2L, "bytes" = 3L, "symbol" = 5L, "any" = 99L)
    i = pmatch(tolower(encoding), names(codes))
    if(is.na(i)) {
      stop("unrecognized encoding")
    }
    
    codes[i]
  }

setMethod("fromJSON", "AsIs",
          function(content, handler = NULL, default.size = 100,
                   depth = 150L, allowComments = TRUE, asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE,  encoding = NA_character_,  stringFun = NULL, ...)  
          {
            fromJSON(as.character(content), handler, default.size, depth, allowComments, asText = TRUE, data, maxChar,
                     simplify = simplify, ..., nullValue = nullValue, simplifyWithNames = simplifyWithNames, encoding = encoding, stringFun = stringFun)  
          })



setMethod("fromJSON", c("character"),
          function(content, handler = NULL,
                   default.size = 100, depth = 150L, allowComments = TRUE, asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE,
                   encoding = NA_character_, stringFun = NULL, ...)  
          {
            if(!asText) {
              content = I(suppressWarnings(paste(readLines(content), collapse = "\n")))
              maxChar = c(0L, nchar(content))
            } else
              content = I(content)
            
            fromJSON(content, handler, default.size, depth, allowComments, asText = FALSE, data, maxChar, simplify = simplify, ...,
                     nullValue = nullValue, simplifyWithNames = simplifyWithNames, encoding = encoding, stringFun = stringFun)
          })


setMethod("fromJSON", c("AsIs", "JSONParserHandler"),
          function(content, handler = NULL,
                   default.size = 100, depth = 150L, allowComments = TRUE, asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE, encoding = NA_character_, stringFun = NULL, ...)  
          {
            fromJSON(content, handler$update, depth = depth, allowComments = allowComments, maxChar = maxChar, simplify = simplify, ..., nullValue = nullValue, simplifyWithNames = simplifyWithNames, encoding = encoding, stringFun = stringFun)
            handler$value()
          })

setMethod("fromJSON", c("AsIs", "function"),
          function(content, handler = NULL,
                   default.size = 100, depth = 150L, allowComments = TRUE, asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE,
                   encoding = NA_character_, stringFun = NULL, ...)  
          {
            oldFromJSON(content, handler, depth = depth, allowComments = allowComments, maxChar = maxChar, simplify = simplify, ..., simplifyWithNames = simplifyWithNames, nullValue = nullValue, encoding = encoding, stringFun = stringFun)
          })

setMethod("fromJSON", c("AsIs", "NativeSymbolInfo"),
          function(content, handler = NULL,
                   default.size = 100, depth = 150L, allowComments = TRUE, asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE,
                   encoding = NA_character_, stringFun = NULL, ...)  
          {
            oldFromJSON(content, handler$address, depth = depth, allowComments = allowComments, data = data, maxChar = maxChar,
                        simplify = simplify, ..., simplifyWithNames = simplifyWithNames, nullValue = nullValue, encoding = encoding, stringFun = stringFun)
          })

oldFromJSON = 
  function(content, handler = NULL,
           default.size = 100, depth = 150L, allowComments = TRUE, asText = isContent(content),
           data = NULL, maxChar = c(0L, nchar(content)), simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE,
           encoding = NA_character_, ...)  
  {
    if(inherits(handler, "NativeSymbol")) {
      data = list(handler, data)
      fun = NULL
    } else
      fun = handler
    
    # Would like to allow the caller specify maxChar and not override it here.
    # But the conversion to raw might yield a vector longer than the number of 
    # characters in x.
    content = substring(content, maxChar[1], maxChar[2])
    cntnt = as.integer(charToRaw(content))
    maxChar = c(0L, length(cntnt))
    
    ans = .Call("R_readFromJSON", cntnt, as.integer(depth), as.logical(allowComments),
                fun, data, maxChar)
    
    if(inherits(handler, "NativeSymbol"))
      data[[2]]
    else
      ans
  }

setMethod("fromJSON", "connection",
          function(content, handler = NULL, default.size = 100,
                   depth = 150L, allowComments = TRUE, asText = isContent(content),
                   data = NULL, maxChar = c(0L, nchar(content)), 
                   simplify = Strict, nullValue = NULL, simplifyWithNames = TRUE, encoding = NA_character_,
                   stringFun = NULL, maxNumLines = -1L, ...)  
          {
            txt = paste(readLines(content, maxNumLines), collapse = "\n")
            #browser()  
            fromJSON(I(txt), handler, default.size, depth, allowComments, asText = TRUE, data = data, maxNumLines = maxNumLines,
                     simplify = simplify, ..., nullValue = nullValue, simplifyWithNames = simplifyWithNames,
                     encoding = encoding, stringFun = stringFun)
          })


if(FALSE) 
  setMethod("fromJSON", "connection",
            # This will be changed so that the code that passes content to the JSON parser
            # calls   readLines(, n = numLines) on the connection
            function(content, handler = NULL, default.size = 100,
                     depth = 150L, allowComments = TRUE, asText = isContent(content),
                     data = NULL, maxNumLines = -1L, ...)  
            {
              handlerFun = inherits(handler, "JSONParserHandler")
              if(handlerFun) {
                fun = handler$update
              } else
                fun = handler
              
              if(inherits(handler, "NativeSymbolInfo"))
                handler = handler$address
              
              if(inherits(handler, "NativeSymbol")) {
                data = list(handler, data)
                fun = NULL
              }
              
              if(!isOpen(content)) {
                open(content, "r")
                on.exit(close(content))
              }
              
              ans = .Call("R_readFromJSON", content, as.integer(depth), as.logical(allowComments),
                          fun, data, as.integer(maxNumLines))
              
              if(inherits(handler, "NativeSymbol"))
                data[[2]]
              else if(handlerFun) {
                handler$value()
              } else
                ans
              
              
              #  fromJSON(content, handler, depth, allowComments, asText = TRUE, data, ...) 
              #   fromJSON(paste(readLines(content), collapse = "\n"), handler, depth, allowComments, asText = TRUE, data, ...)
            })