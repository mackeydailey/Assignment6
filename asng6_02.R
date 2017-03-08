
setClass("ExprC", representation())
setClass("BoolC", representation(bool = "logical"), contains = "ExprC") 
setClass("IdC", representation(id = "character"), contains = "ExprC")
setClass("NumC", representation(num = "numeric"), contains = "ExprC")
setClass("IfC", representation(a = "ExprC", b = "ExprC", c = "ExprC"), contains = "ExprC")
setClass("StringC", representation(str = "character"), contains = "ExprC")
setClass("BinOp", representation(op = "character", left = "ExprC", right = "ExprC"), contains = "ExprC")
setClass("LamC", representation(ids = "list", body = "ExprC"), contains = "ExprC")
setClass("AppC", representation(id = "ExprC", exprs = "list"), contains = "ExprC")

setClass("Value", representation())
setClass("EnV", representation(bindings = "list"))
setClass("Binding", representation(id = "character", value = "Value"))

setClass("NumV", representation(num = "numeric"), contains = "Value")
setClass("BoolV", representation(bool = "logical"), contains = "Value")
setClass("StringV", representation(str = "character"), contains = "Value")
setClass("CloV", representation(args = "list", body = "ExprC", env = "EnV"), contains = "Value")
setClass("PrimV", representation(fields = "function"), contains = "Value")
setClass("ErrV", representation(e = "character"), contains = "Value")

interp <- function(input, envir) {
  if (is(envir, "EnV")) {
    if(is(input, "NumC")) {
      return(new("NumV", num = input@num))
    } else if (is(input, "BoolC")) {
      return(new("BoolV", bool = input@bool))
    } else if (is(input, "IdC")) {
      return(lookup(input@id, envir))
    } else if (is(input, "LamC")) {
      return(new("CloV", args = input@ids, body = input@body, env = envir))
    } else if (is(input, "IfC")) {
      return(interp.IfC(input, envir))
    } else if (is(input, "BinOp")) {
      return(interp.BinOp(input, envir))
    } else if (is(input, "AppC")) {
      fd <- interp(input@id, envir) #get closure
      argvals <- interp.args(input@exprs, envir) #get list of vals
      newenv <- new("EnV", bindings=build.env(fd@args, argvals, fd@env))
      print(newenv)
      return(interp(fd@body, newenv))
    } else if (is(input, "StringC")) {
      return(new("StringV", str = input@str))
    }
    else {
      return(new("ErrV", e = "Unknown data type"))
    }
  } else {
    return(new("ErrV", e = "Second argument not an Env"))
  }
  
}

lookup <- function(id, envir) {
  for (i in 1:(length(envir))) {
   if(slot(envir, "bindings")[[i]]@id == id)
     return(slot(envir, "bindings")[[i]]@value)
  }
  return(new("ErrV", e = "Function not in the current environment"))
}

interp.args <- function(args, envir) {
  arglist <- vector("list", length(args))
  for (i in 1:(length(args))) {
    arglist[[i]] <- interp(args[[i]], envir)
  }
  return(arglist)
}

interp.IfC <- function(input, envir) {
  aRes <- interp(input@a, envir)
  if(is(aRes, "ErrV")) {
    return(aRes)
  }
  
  if (is(aRes, "BoolV")) {
    if (aRes@bool) {
      return(interp(input@b, envir))
    } else {
      return(interp(input@c, envir))
    }
  } else {
    return(new("ErrV", e = "Error: IfC - did not return a boolean"))
  }
}

interp.BinOp <- function(input, envir) {
  op <- input@op
  if (is(op, "ErrV")) {
    return(op)
  }
  left <- interp(input@left, envir)
  if (is(left, "ErrV")) {
    return(left)
  }
  right <- interp(input@right, envir)
  if (is(right, "ErrV")) {
    return(right)
  }
  
  if (is.character(op)) {
    if (op == "eq?") {
      if (is(left, "NumV") && is(right, "NumV")) {
        return(new("BoolV", bool = left@num == right@num))
      } else if (is(left, "BoolV") && is(right, "BoolV")) {
        return(new("BoolV", bool = left@bool == right@bool))
      } else if (is(left, "StringV") && is(right, "StringV")) {
        return(new("BoolV", bool = left@str == right@str))
      } else {
        return(new("BoolV", bool = FALSE))
      }
    } else if (is(left, "NumV") && is(right, "NumV")) {
      if (op == "+") {
        return(new("NumV", num = left@num + right@num)) 
      } else if (grepl(op, "-")) {
        return(new("NumV", num = left@num - right@num)) 
      } else if (grepl(op, "/")) {
        if (right@num == 0) {
          return(new("ErrV", e = "Error: Binop - divided by 0"))
        } else {
          return(new("NumV", num = left@num / right@num))
        }
      } else if (grepl(op, "*")) {
        return(new("NumV", num = left@num * right@num))
      } else if (grepl(op, "<=")) {
        return(new("BoolV", bool = left@num <= right@num))
      } else 
        return(new("ErrV", e ="Error: Binop - unknown operator"))
    }
    else {
      return(new("ErrV", e = "Error: Binop - didn't return numbers"))
    }
  } else {
    return(new("ErrV", e = "Error: Binop - invalid operator"))
  }
}

build.env <- function(cloargs, args, envir) {
  newenv <- vector("list", length(args))
  for (i in 1:(length(args))) {
    newenv[[i]] <- new("Binding", id=cloargs[[i]], value=args[[i]])
  }
  ret <- (c(envir@bindings, newenv))
  return(ret)
}

serialize <- function(val) {
  UseMethod("serialize", val)
}

serialize.ErrV <- function(val) {
  print(val@e)
}

serialize.default <- function(val) {
  print("Error: Unknown value")
}

serialize.StringV <- function(val) {
  return(val@str)
}

serialize.BoolV <- function(val) {
  return(val@bool)
}

serialize.NumV <- function(val) {
  return(val@num)
}

serialize.CloV <- function(val) {
  return("#<procedure>")
}


serialize(interp(new("IfC", 
                     a = new("BoolC", bool = TRUE), 
                     b = new("BinOp", op = "/", left = new("NumC", num = 8), right = new("NumC", num = 0)), 
                     c = new("BinOp", op = "eq?", left = new("IdC", id = "d"), right = new("NumC", num = 8)))))

serialize(interp(new("BinOp", op = "eq?", left = new("NumC", num = 8), right = new("NumC", num = 8)), new("EnV", bindings = list())))
 
interp.args(list(new("NumC", num = 3), 
                 new("BinOp", op = "+", left = new("NumC", num = 4), 
                     right = new("NumC", num = 8))),
            new("EnV", bindings = list()))

build.env(list("a", "b", "c"), list(new("NumV", num=3), new("NumV", num=4), new("NumV", num=5)), new("EnV", bindings = list()))

lookup("a", new("EnV", bindings = list(new("Binding", id="a", value=new("NumV", num=4)), new("Binding", id="b", value=new("NumV", num=5)))))

testenv <- new("EnV", bindings=list(new("Binding", id="f", value=new("CloV", args=list("x"), body=new("BinOp", op="+", left=new("IdC", id="x"), right=new("NumC", num=2)), env=new("EnV", bindings=list())))))

interp(new("AppC", id=new("IdC", id="f"), exprs=list(new("NumC", num=3))), testenv)
