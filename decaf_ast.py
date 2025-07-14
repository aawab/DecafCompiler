# Vincent Zheng
# Aawab Mahmood

# Global vars to keep track of curr method, class, etc. for typechecking

currentClass = None
currentMethod = None
errorFound = None

# Define AST classes for each language construct

classTable = dict()

# Methods for filling class table and searching in it, alongside printing it

def addClassToTable(cname, cval):
    classTable[cname]=cval

def searchForClass(cname):
    if cname in classTable:
        return classTable[cname]
    else:
        return None

def printClassTable():
    for cname in classTable:
        print(classTable[cname])
    print('--------------------------------------------------------------------------')

def initialize():
    IN = Class("In", None, True)
    OUT = Class("Out", None, True)

    scanInt = Method('scan_int', IN.name, 'public', 'static', Type('int'))
    IN.addMethod(scanInt)

    scanFloat = Method('scan_float', IN.name, 'public', 'static', Type('float'))
    IN.addMethod(scanFloat)

    printInt = Method('print', OUT.name, 'public', 'static', Type('void'))
    printInt.addVar(Variable('i', 'formal', Type('int')))
    OUT.addMethod(printInt)

    printFloat = Method('print', OUT.name, 'public', 'static', Type('void'))
    printFloat.addVar(Variable('i', 'formal', Type('float')))
    OUT.addMethod(printFloat)

    printBool = Method('print', OUT.name, 'public', 'static', Type('void'))
    printBool.addVar(Variable('i', 'formal', Type('boolean')))
    OUT.addMethod(printBool)

    printString = Method('print', OUT.name, 'public', 'static', Type('void'))
    printString.addVar(Variable('i', 'formal', Type('string')))
    OUT.addMethod(printString)

    addClassToTable("In", IN)
    addClassToTable("Out", OUT)

def typeCheck():
    global errorFound
    errorFound=False
    for cname in classTable:
        c = classTable[cname]
        c.typeCheck()
    return not errorFound

def typeErrorFound(errorMsg, startLineno, endLineno):
    global errorFound

    if errorFound!=True:
        errorFound = True
        print(f'ERROR: {errorMsg}!')

        if startLineno == endLineno:
            print(f'ERROR: Found on line {startLineno}.')
        else:
            print(f'ERROR: Found on lines {startLineno} to {endLineno}.')


def resolveFieldName(appli, baseClass, fname, currentClass):
    while baseClass!=None:
        f = baseClass.searchForField(fname)
        if (f!=None and f.appli == appli) and (f.visi == 'public' or baseClass==currentClass):
            return f
        else:
            baseClass = baseClass.superClass
    return None
    
def resolveMethodName(appli, baseClass, mname, argTypes, currentClass):
    while baseClass!=None:
        m = baseClass.searchForMethod(mname, argTypes)

        if (m!=None and m.appli == appli) and (m.visi == 'public' or baseClass==currentClass):
            return m
        else:
            baseClass = baseClass.superClass
    return None

def resolveConstructorName(argTypes, baseClass, currentClass):
    while baseClass!=None:
        c = baseClass.searchForConstructor(argTypes)

        if c!=None and (c.visi == 'public' or baseClass==currentClass):
            return c
        else:
            baseClass = baseClass.superClass
    return None

class Class():
    def __init__(self, n, sc, builtIn=False):
        self.name=n
        self.superClass=sc
        self.builtIn = builtIn
        self.fields = dict()
        self.staticFields = []
        self.instanceFields = []
        self.constructors = []
        self.methods = []

    def __str__(self) -> str:
        if self.name == "In" or self.name == "Out":
            return ""
        res = '--------------------------------------------------------------------------\n'
        res+= f'Class name: {self.name}\n'

        if self.superClass==None:
            scname = ""
        else:
            scname = self.superClass.name

        res+= f'Super class Name: {scname}\n'
        res+= 'Fields:\n'
        for field in self.fields.values():
            res+=str(field)
        res+= 'Constructors:\n'
        for constructor in self.constructors:
            res+=str(constructor)
        res+= 'Methods:\n'
        for method in self.methods:
            res+=str(method)
        return res
    
    # Methods for filling out class info

    def addField(self, name, val):
        self.fields[name]=val
        if val.appli == 'static':
            self.staticFields.append(val)
        else:
            self.instanceFields.append(val)

    def addMethod(self, method):
        self.methods.append(method)

    def addConstructor(self, constructor):
        self.constructors.append(constructor)
    
    # Method for checking if field exists
        
    def fieldIndex(self, field):
        index = 0
        try:
            index = self.instanceFields.index(field)
        except:
            self.superClass.fieldIndex(field)
        return index

    def searchForField(self, name):
        if name in self.fields:
            return self.fields[name]
        else:
            return None
    
    def searchForMethod(self, name, paramTypes):
        for method in self.methods:
            if method.name == name:
                # Check that signature is the same
                if len(method.getFormals()) != len(paramTypes):
                    continue

                allFormalsMatch = True
                for formal, param in zip(method.getFormals(), paramTypes):
                    if not param.isSubtypeOf(formal.type):
                        allFormalsMatch = False

                if allFormalsMatch:
                    return method
        
        return None

    def searchForConstructor(self, paramTypes):
        for constructor in self.constructors:
            # Check that signature is the same
            if len(constructor.getFormals()) != len(paramTypes):
                continue

            allFormalsMatch = True
            for formal, param in zip(constructor.getFormals(), paramTypes):
                if not param.isSubtypeOf(formal.type):
                    allFormalsMatch = False

            if allFormalsMatch:
                return constructor
        
        return None

    # Method for checking subclasses

    def isSubClassOf(self, otherClass):
        if self.name == otherClass.name:
            return True
        elif self.superClass != None:
            if self.superClass == otherClass:
                return True
            else:
                return self.superClass.isSubClassOf(otherClass)
        return False
        
    # Typecheck the class
    def typeCheck(self):
        if self.builtIn:
            return
        global currentClass
        currentClass = self

        # No need for overload checking, just typecheck constructors/methods

        for con in self.constructors:
            con.typeCheck()
        
        for meth in self.methods:
            meth.typeCheck()

class Constructor():
    counter=0
    
    def __init__(self, n, visi):
        Constructor.counter+=1
        self.id=Constructor.counter
        self.name = n
        self.visi = visi
        self.vars = dict()
        self.body = Block(None, None, [Skip_Statement(None, None)])
    
    def searchForVar(self, name):
        if name in self.vars:
            return self.vars[name]
        
        return None

    def addVar(self, var):
        self.vars[var.name] = var
        var.setID(len(self.vars))
        
    def getFormals(self):
        formals = []

        for var in self.vars.values():
            if var.kind == 'formal':
                formals.append(var)
        
        return formals
    
    def __str__(self) -> str:
        res = f'CONSTRUCTOR: {self.id}, {self.visi}\n'
        res += f'Constructor parameters: '
        res += ''.join(f'{v.id}, ' for v in self.vars.values() if v.kind=="formal")[:-2] + '\n'
        res += f'Variable Table:\n'
        for var in self.vars.values():
            res+=str(var)
        res += 'Constructor Body:\n'
        res += str(self.body)
        return res

    def typeCheck(self):
        self.body.typeCheck()

class Method():
    counter=0
    
    def __init__(self, n, cname, visi, appli, type):
        Method.counter+=1
        self.id=Method.counter
        self.name = n
        self.cname = cname
        self.visi = visi
        self.appli = appli
        self.type = type
        self.vars = dict()
        self.body = Block(None, None, [Skip_Statement(None, None)])
    
    def searchForVar(self, name):
        if name in self.vars:
            return self.vars[name]
        
        return None
    
    def getFormals(self):
        formals = []

        for var in self.vars.values():
            if var.kind == 'formal':
                formals.append(var)
        
        return formals

    def addVar(self, var):
        self.vars[var.name] = var
        var.setID(len(self.vars))
    
    def __str__(self) -> str:
        res = f'METHOD: {self.id}, {self.name}, {self.cname}, {self.visi}, {self.appli}, {self.type}\n'
        res += f'Method parameters: '
        res += ''.join(f'{v.id}, ' for v in self.vars.values() if v.kind=="formal")[:-2] + '\n'
        res += f'Variable Table:\n'
        for var in self.vars.values():
            res+=str(var)
        res += 'Method Body:\n'
        res += str(self.body)
        return res

    def typeCheck(self):
        global currentMethod
        currentMethod = self
        self.body.typeCheck()
    
class Field():
    counter = 0

    def __init__(self, n, cname, visi, appli, type):
        Field.counter+=1
        self.id = Field.counter
        self.name = n
        self.cname = cname
        self.visi = visi
        self.appli = appli
        self.type = type

    def __str__(self) -> str:
        return f'FIELD {self.id}, {self.name}, {self.cname}, {self.visi}, {self.appli}, {self.type}\n'
         
class Variable():
    def __init__(self, n, k, t):
        self.name=n
        self.id=0
        self.kind=k
        self.type=t

    def setID(self, id):
        self.id=id
    
    def __str__(self) -> str:
        return f'Variable {self.id}, {self.name}, {self.kind}, {self.type}\n'

class Type():
    def __init__(self, t,lit = False):
        if t in ['int', 'float', 'boolean', 'string', 'void', 'error', 'null']:
            self.kind = 'basic'
            self.type = t
        elif not lit :
            self.kind = 'user'
            self.baseClass = t
        else:
            self.kind = 'class-literal'
            self.baseClass = t
    
    def __str__(self) -> str:
        if self.kind == 'user':
            return f'user({self.baseClass.name})'
        elif self.kind == 'class-literal':
            return f'class-literal({self.baseClass.name})'
        else:
            return self.type

    def isSubtypeOf(self, otherType):
        if self.kind == 'basic':
            if otherType.kind == 'basic':
                if (self.type == otherType.type) or (self.type == 'int' and otherType.type == 'float'):
                    return True
            elif self.type == 'null':
                return otherType.kind == 'user'
            
        elif (self.kind == 'user' and otherType.kind == 'user') or (self.kind == 'class-literal' and otherType.kind == 'class-literal'):
            return self.baseClass.isSubClassOf(otherType.baseClass)
        
        return False
    
    def isInt(self):
        return self.kind == 'basic' and self.type == 'int'
    
    def isFloat(self):
        return self.kind == 'basic' and self.type == 'float'
    
    def isNumber(self):
        return self.kind == 'basic' and (self.type == 'int' or self.type == 'float')

    def isBool(self):
        return self.kind == 'basic' and self.type == 'boolean'

    def isWorking(self):
        return not (self.kind == 'basic' and self.type == 'error')
    
class Statement():
    def __init__(self, start, end):
        self.start = start
        self.end = end

class Return_Statement(Statement):
    def __init__(self, start, end, returnExpr=None):
        super().__init__(start, end)
        
        self.start = start
        self.end = end
        self.returnExpr = returnExpr
        self.typeCorrect = None
    
    def __str__(self) -> str:
        return f'Return-stmt({self.returnExpr})'

    def typeCheck(self):
        global currentMethod
        if self.typeCorrect==None:
            if self.returnExpr==None:
                returnType = Type('void')
            else:
                returnType = self.returnExpr.typeCheck()
            self.typeCorrect = returnType.isSubtypeOf(currentMethod.type)

            if returnType.isWorking() and not self.typeCorrect:
                typeErrorFound('Type error in method return statement', self.start, self.end)
                self.typeCorrect = False

        return self.typeCorrect

class Block(Statement):
    def __init__(self, start, end, content) -> None:
        super().__init__(start, end)
        
        self.statements = content
        self.typeCorrect = None

    def add(self, expr):
        self.statements.append(expr)

    def typeCheck(self):
        if self.typeCorrect==None:
            self.typeCorrect = all([stmt.typeCheck() for stmt in self.statements])
        return self.typeCorrect
    
    def __str__(self) -> str:
        stringified = 'Block([\n'
        
        stringified +='\n,\t'.join(str(statement)for statement in self.statements)
        stringified += '\n])\n'

        return stringified

class If_Statement(Statement):
    def __init__(self, start, end, condition, thenBlock, elseBlock):
        super().__init__(start, end)
        
        self.condition = condition
        self.thenBlock = thenBlock
        self.elseBlock = elseBlock
        self.typeCorrect = None
    
    def typeCheck(self):
        conditionType = self.condition.typeCheck()
        if not conditionType.isBool():
            typeErrorFound('If statement condition not boolean', self.condition.start, self.condition.end)
            self.typeCorrect = False
            return False
        
        thenBlockCheck = self.thenBlock.typeCheck()
        if thenBlockCheck == False or (type(thenBlockCheck) != bool and not thenBlockCheck.isWorking()):
            typeErrorFound('If statement then block not type correct', self.thenBlock.start, self.thenBlock.end)
            self.typeCorrect = False
            return False
        
        elseBlockCheck = self.elseBlock.typeCheck()
        if elseBlockCheck == False or (type(elseBlockCheck) != bool and not elseBlockCheck.isWorking()):
            typeErrorFound('If statement else block not type correct', self.elseBlock.start, self.elseBlock.end)
            self.typeCorrect = False
            return False
        
        self.typeCorrect = True
        return True
    
    def __str__(self) -> str:
        return f'If-stmt({self.condition}, {self.thenBlock}, {self.elseBlock})'

class While_Statement(Statement):
    def __init__(self, start, end, condition, body):
        super().__init__(start, end)
        
        self.condition = condition
        self.body = body
        self.typeCorrect = None
    
    def typeCheck(self):
        conditionType = self.condition.typeCheck()
        if not conditionType.isBool():
            typeErrorFound('While statement condition not boolean', self.condition.start, self.condition.end)
            self.typeCorrect = False
            return False
        
        bodyBlockCheck = self.body.typeCheck()
        if bodyBlockCheck == False or (type(bodyBlockCheck) != bool and not bodyBlockCheck.isWorking()):
            typeErrorFound('While statement body not type correct', self.body.start, self.body.end)
            self.typeCorrect = False
            return False
        
        self.typeCorrect = True
        return True
    
    def __str__(self) -> str:
        return f'While-stmt({self.condition}, {self.body})'

class For_Statement(Statement):
    def __init__(self, start, end, initializer, loopCondition, updater, body):
        super().__init__(start, end)
        
        self.initializer = initializer
        self.loopCondition = loopCondition
        self.updater = updater
        self.body = body
        self.typeCorrect = None

    def __str__(self) -> str:
        return f'For-stmt({self.initializer}, {self.loopCondition}, {self.updater}, {self.body})'
    
    def typeCheck(self):
        global errorFound

        if self.typeCorrect == None:
            conditionCheck = self.loopCondition.typeCheck()
            if not conditionCheck.isBool():
                typeErrorFound('For loop condition is not boolean', self.loopCondition.start, self.loopCondition.end)
                self.typeCorrect = False
                return False

            initializerCheck = self.initializer.typeCheck()
            if not initializerCheck.isWorking():
                typeErrorFound('For loop initializer isn\'t type correct', self.initializer.start, self.initializer.end)
                self.typeCorrect = False
                return False
            
            updaterCheck = self.updater.typeCheck()
            if not updaterCheck.isWorking():
                typeErrorFound('For loop updater isn\'t type correct', self.updater.start, self.updater.end)
                self.typeCorrect = False
                return False

            bodyCheck = self.body.typeCheck()
            if bodyCheck == False or (type(bodyCheck) != bool and not bodyCheck.isWorking()):
                typeErrorFound('For loop body is not type correct', self.body.start, self.body.end)
                self.typeCorrect = False
                return False

            self.typeCorrect = True

        return self.typeCorrect

class Break_Statement(Statement):
    def __init__(self, start, end):
        super().__init__(start, end)

        self.typeCorrect = True

    def typeCheck(self):
        return self.typeCorrect
    
    def __str__(self) -> str:
        return f'Break-stmt()'

class Continue_Statement(Statement):
    def __init__(self, start, end):
        super().__init__(start, end)
        
        self.typeCorrect = True

    def typeCheck(self):
        return self.typeCorrect
    
    def __str__(self) -> str:
        return f'Continue-stmt()'

class Skip_Statement(Statement):
    def __init__(self, start, end):
        super().__init__(start, end)
        
        self.typeCorrect = True

    def typeCheck(self):
        return self.typeCorrect

    def __str__(self) -> str:
        return f'Skip-stmt'

class Expr_Statement(Statement):
    def __init__(self, start, end, expr):
        super().__init__(start, end)
        
        self.expr = expr
        self.typeCorrect = None
    
    def typeCheck(self):
        if self.typeCorrect==None:
            exprTypeCheck = self.expr.typeCheck()

            if not exprTypeCheck.isWorking():
                typeErrorFound('Type error found in expression statement', self.start, self.end)
                self.typeCorrect = False
            else:
                self.typeCorrect = True
        
        return self.typeCorrect
    
    def __str__(self) -> str:
        return f'Expr-stmt({self.expr})'

class Expression():
    def __init__(self, start, end):
        self.start = start
        self.end = end

class Const_Expression(Expression):
    def __init__(self, start, end, val, valKind):
        super().__init__(start, end)
        
        self.kind = valKind
        self.val = val
        self.type = None

    def __str__(self) -> str:
        if self.kind == 'LITER_INT':
            return f'Constant(Integer-constant({self.val}))'
        elif self.kind == 'LITER_FLOAT':
            return f'Constant(Float-constant({self.val}))'
        elif self.kind == 'LITER_STRING':
            return f'Constant(String-constant({self.val}))'
        elif self.kind == 'NULL':
            return f'Constant(Null)'
        elif self.kind == 'TRUE':
            return f'Constant(True)'
        elif self.kind == 'FALSE':
            return f'Constant(False)'
    
    def typeCheck(self):
        if self.type == None:
            if self.kind == 'LITER_INT':
                self.type = Type('int')
            elif self.kind == 'LITER_FLOAT':
                self.type = Type('float')
            elif self.kind == 'LITER_STRING':
                self.type = Type('string')
            elif self.kind == 'NULL':
                self.type = Type('null')
            elif self.kind == 'TRUE' or self.kind == 'FALSE':
                self.type = Type('boolean')
        return self.type

class Var_Expression(Expression):
    def __init__(self, start, end, var):
        super().__init__(start, end)
        
        self.var = var
        self.type = None
        self.objID = None
    
    def __str__(self) -> str:
        return f'Variable({self.var.id})'
    
    def typeCheck(self):
        if self.type == None:
            self.type = self.var.type

        return self.type

class Unary_Expression(Expression):
    def __init__(self, start, end, operand, operator):
        super().__init__(start, end)
        
        self.operand = operand
        self.operator = operator
        self.type = None

    def __str__(self) -> str:
        return f'Unary({self.operator}, {self.operand})'
    
    def typeCheck(self):
        global errorFound
        if self.type == None:
            exprType = self.operand.typeCheck()
            self.type = Type('error')
            if self.operator == '-':
                if exprType.isNumber():
                    self.type = exprType
                else:
                    typeErrorFound('Unary minus expr needs an int/float type', self.start, self.end)

            elif self.operator == '!':
                if exprType.isBool():
                    self.type = exprType
                else:
                    typeErrorFound('Unary negation expr needs a boolean type', self.start, self.end)
        
        return self.type

class Assign_Expression(Expression):
    def __init__(self, start, end, lhs, rhs):
        super().__init__(start, end)
        
        self.lhs = lhs
        self.rhs = rhs
        self.type = None
        self.objID = None

        if type(self.rhs) == New_Object_Expression:
            if type(self.lhs) == Var_Expression:
                self.rhs.objID = self.lhs.var.id
                self.lhs.objID = self.lhs.var.id
            else:
                id = self.lhs.base.var.id
                self.rhs.objID = id
                self.lhs.objID = id
        elif type(self.rhs) == Method_Call_Expression:
            if type(self.lhs) == Var_Expression:
                self.rhs.objID = self.lhs.var.id
                self.lhs.objID = self.lhs.var.id
            else:
                id = self.lhs.base.var.id
                self.rhs.objID = id
                self.lhs.objID = id

    def __str__(self) -> str:
        return f'Assign({self.lhs}, {self.rhs}, {self.lhs.typeCheck()}, {self.rhs.typeCheck()})'

    def typeCheck(self):
        global errorFound
        if self.type==None:
            lhsType = self.lhs.typeCheck()
            rhsType = self.rhs.typeCheck()
            if lhsType.isWorking() and rhsType.isWorking():
                if rhsType.isSubtypeOf(lhsType):
                    self.type = rhsType
                else:
                    typeErrorFound('Assign expression has type error, rhs type must be subtype of lhs', self.start, self.end)
                    self.type=Type('error')
            else:
                typeErrorFound('Assign expression has type error', self.start, self.end)
                self.type=Type('error')

        return self.type

class Auto_Expression(Expression):
    def __init__(self, start, end, var, op, fix):
        super().__init__(start, end)
        
        self.var = var
        self.op = op
        self.fix = fix
        self.type = None

    def __str__(self) -> str:
        return f'Auto({self.var}, {self.op}, {self.fix})'
    
    def typeCheck(self):
        global errorFound
        if self.type == None:
            varType = self.var.typeCheck()
            if varType.isNumber():
                self.type = varType
            else:
                self.type = Type('error')
                if varType.isWorking():
                    typeErrorFound('Auto expression has type error! Requires an int/float', self.start, self.end)
        
        return self.type          

class Field_Access_Expression(Expression):
    def __init__(self, start, end, base, fieldName):
        super().__init__(start, end)
        
        self.base = base
        self.fieldName = fieldName
        self.fieldObj = None
        self.type = None
        self.objID = None

    def typeCheck(self):
        global errorFound, currentClass
        if self.type == None:
            baseType = self.base.typeCheck()

            if baseType.isWorking():
                if baseType.kind not in ['user', 'class-literal']:
                    typeErrorFound(f'Field access expected user defined class/instance type, but found {baseType.type}', self.start, self.end)
                    self.type= Type('error')
                else:
                    if baseType.kind == 'user':
                        appli = 'instance'
                    else:
                        appli = 'static'
                    baseClass = baseType.baseClass
                    field = resolveFieldName(appli, baseClass, self.fieldName, currentClass)
                    if field == None:
                        typeErrorFound(f'No field named {self.fieldName} is accessible in class {baseClass.name}', self.start, self.end)
                        self.type=Type('error')

                    else:
                        self.fieldObj = field
                        self.type = field.type
        
        return self.type
    
    def __str__(self) -> str:
        return f'Field-access({self.base}, {self.fieldName}, {self.fieldObj.id})'

class Method_Call_Expression(Expression):
    def __init__(self, start, end, field, arguments=[]):
        super().__init__(start, end)
        
        self.base = field.base
        self.methodName = field.fieldName
        self.methodObj = None
        self.arguments = arguments
        self.objID =None
        self.type = None
    
    def typeCheck(self):
        global errorFound, currentClass
        if self.type == None:
            baseType = self.base.typeCheck()

            if baseType.isWorking():
                if baseType.kind not in ['user', 'class-literal']:
                    typeErrorFound('Method call expects a user or class-literal as base, not found.', self.start, self.end)
                    self.type = Type('error')

                else:
                    if baseType.kind == 'user':
                        appli = 'instance'
                    else:
                        appli = 'static'
                    baseClass = baseType.baseClass
                    argTypes = [a.typeCheck() for a in self.arguments]
                    if all([a.isWorking() for a in argTypes]):
                        meth = resolveMethodName(appli, baseClass, self.methodName, argTypes, currentClass)

                        if meth == None:
                            typeErrorFound('Method call not mapped to a method with same signature', self.start, self.end)
                            self.type = Type('error')
                            
                        else:
                            self.methodObj = meth
                            self.type = meth.type
                    else:
                        self.type = Type('error')
        return self.type

    def __str__(self) -> str:
        argStr = ""
        for arg in self.arguments:
            argStr+=str(arg)
        return f'Method-call({self.base}, {self.methodName}, [{argStr}])'

class New_Object_Expression(Expression):
    def __init__(self, start, end, base, arguments=[]):
        super().__init__(start, end)
        
        self.base = base
        self.arguments = arguments
        self.type = None
        self.objID = None
    
    def typeCheck(self):
        global errorFound, currentClass

        if self.type == None:
            paramTypes = [ param.typeCheck() for param in self.arguments ]

            if all([ param.isWorking() for param in paramTypes ]):
                constructor = resolveConstructorName(paramTypes, self.base, currentClass)
                
                if constructor != None:
                    self.type = Type(self.base)
                else:
                    typeErrorFound('Constructor of same signature not found', self.start, self.end)
                    self.type = Type('error')

            else:
                typeErrorFound('New object expression constructor parameter not type correct', self.start, self.end)
                self.type = Type('error')

        return self.type

    def __str__(self) -> str:
        argStr = ""
        for arg in self.arguments:
            argStr+=str(arg)
        return f'New-object({self.base.name}, [{argStr}])'

class This_Expression(Expression):
    def __init__(self, start, end):
        super().__init__(start, end)
        
        self.type = None
        self.currClass = None
    
    def typeCheck(self):
        global errorFound, currentClass
        if self.type == None:
            self.currClass = currentClass
            self.type = Type(currentClass)

        return self.type

    def __str__(self) -> str:
        return 'This'

class Super_Expression(Expression):
    def __init__(self, start, end):
        super().__init__(start, end)
        
        self.type = None
        self.currClass = None
    
    def typeCheck(self):
        global currentClass, errorFound
        if self.type == None:
            if currentClass.superClass!=None:
                self.currClass = currentClass
                self.type = Type(currentClass.superClass)
            else:
                typeErrorFound(f'Super called but current class {self.currClass.name} has no superclass', self.start, self.end)
                self.type = Type('error')
                
        return self.type

    def __str__(self) -> str:
        return 'Super'

class Class_Ref_Expression(Expression):
    def __init__(self, start, end, cref):
        super().__init__(start, end)
        
        self.cref = cref
        self.type = None
    
    def typeCheck(self):
        if self.type == None:
            self.type = Type(self.cref, True)
        
        return self.type

    def __str__(self) -> str:
        return f'Class-reference({self.cref.name})'

class Bin_Expression(Expression):
    def __init__(self, start, end, val1, val2, op):
        super().__init__(start, end)
        
        self.val1 = val1
        self.val2 = val2
        self.op = op
        self.type = None

    def __str__(self) -> str:
        return f'Binary({self.op}, {self.val1}, {self.val2})'
    
    def typeCheck(self):
        global errorFound
        if self.type == None:
            val1Type = self.val1.typeCheck()
            val2Type = self.val2.typeCheck()
            self.type = Type('error')

            if self.op in ['+', '-', '*', '/']:
                if val1Type.isInt() and val2Type.isInt():
                    self.type = val1Type
                elif val1Type.isNumber() and val2Type.isNumber():
                    self.type = Type('float')
                else:
                    if val1Type.isWorking() and val2Type.isWorking():
                        typeErrorFound('Binary arith operation requires two args that are ints or floats', self.start, self.end)

            elif self.op in ['&&', '||']:
                if val1Type.isBool() and val2Type.isBool():
                    self.type = Type('boolean')
                else:
                     if val1Type.isWorking() and val2Type.isWorking():
                        typeErrorFound('Binary boolean operation requires two args that are booleans', self.start, self.end)

            elif self.op in ['<','>', '<=','>=']:
                if val1Type.isNumber() and val2Type.isNumber():
                    self.type = Type('boolean')
                else:
                    if val1Type.isWorking() and val2Type.isWorking():
                        typeErrorFound('Binary arith comparison requires two args that are ints or floats', self.start, self.end)

            else:
                if val1Type.isWorking() and val2Type.isWorking():
                    if val1Type.isSubtypeOf(val2Type) or val2Type.isSubtypeOf(val1Type):
                        self.type = Type('boolean')
                    else:
                        typeErrorFound('Binary eq/uneq comparison needs compatible types', self.start, self.end)

        return self.type

class LinkedList():
    def __init__(self, val) -> None:
        self.val = val
        self.next = None
    
    def addNext(self, nextNode):
        self.next = nextNode
