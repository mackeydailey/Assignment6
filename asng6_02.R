
setClass("ExprC", representation())
setClass("BoolC", representation(bool = "logical"), contains = "ExprC") 
setClass("IdC", representation(id = "character"), contains = "ExprC")
setClass("NumC", representation(num = "numeric"), contains = "ExprC")
setClass("IfC", representation(a = "ExprC", b = "ExprC", c = "ExprC"), contains = "ExprC")
setClass("StringC", representation(str = "character"), contains = "ExprC")
setClass("BinOp", representation(op = "character", left = "ExprC", right = "ExprC"), contains = "ExprC")
setClass("LamC", representation(ids = "list", body = "ExprC", contains = "ExprC"))

setClass("Value", representation())
setClass("EnV", representation(bindings = "list"))
setClass("Binding", representation(id = "character", value = "Value"))

setClass("NumV", representation(num = "numeric"), contains = "Value")
setClass("BoolV", representation(bool = "logical"), contains = "Value")
setClass("StringV", representation(str = "character"), contains = "Value")
setClass("CloV", representation(args = "list", body = "ExprC", env = "EnV", contains = "Value"))
setClass("ErrV", representation(e = "character"), contains = "Value")

interp <- function(input) {
  UseMethod("interp", input)
}

interp.default <- function(input) {
  return(new("ErrV", e = "Unable to interp..."))
}
interp.NumC <- function(input) {
  return(new("NumV", num = input@num)) 
}
interp.BoolC <- function(input) {
  return(new("BoolV", bool = input@bool))
}
interp.IdC <- function(input) { ##TODO lookup in environment
  return(input@id)
}
interp.StringC <- function(input) {
  return(new("StringV", str = input@str))
}
interp.LamC <- function(input) { ##TODO check valid args and get new EnV
  return(new("CloV", input@ids, input@body, new("EnV", list())))
}

interp.IfC <- function(input) {
  aRes <- interp(input@a)
  if(is(aRes, "ErrV")) {
    return(aRes)
  }
  
  if (is(aRes, "BoolV")) {
    if (aRes@bool) {
      return(interp(input@b))
    } else {
      return(interp(input@c))
    }
  } else {
    return(new("ErrV", e = "Error: IfC - did not return a boolean"))
  }
}

compareExprC <- function(left, right) {
  if (is(left, "NumV") && is(right, "NumV")) {
    return(new("BoolV", bool = left@num == right@num))
  } else if (is(left, "BoolV") && is(right, "BoolV")) {
    return(new("BoolV", bool = left@bool == right@bool))
  } else if (is(left, "StringV") && is(right, "StringV")) {
    return(new("BoolV", bool = left@str == right@str))
  } else {
    return(new("BoolV", bool = FALSE))
  }
}

interp.BinOp <- function(input) {
  op <- input@op
  left <- interp(input@left)
  if (is(left, "ErrV")) {
    return(left)
  }
  right <- interp(input@right)
  if (is(right, "ErrV")) {
    return(right)
  }
  
  if (is.character(op)) {
    if (op == "eq?") {
      compareExprC(left, right)
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


serialize(interp(new("BinOp", op = "eq?", left = new("NumC", num = 8), right = new("NumC", num = 8))))
