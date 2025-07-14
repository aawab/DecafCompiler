# Vincent Zheng, vinzheng, 113469839
# Aawab Mahmood, aamahmood, 113472709

# Responsible for using absmc funcs correctly to use registers, stacks, etc.
# and build strings consisting of the program instructions\

from decaf_absmc import abstractMachine
from decaf_ast import *

am = None

# After typechecking is done from decaf_checker, prints classtable and then comes here to initialize abstract machine and
# generate the code, finally using am.printProgram() to print and output to a new file
def generateCode(fn):
    global am
    am = abstractMachine()
    am.fillStaticArea()
    am.outputProgram+=generateClassCode(classTable)
    am.printProgram(fn)

# DONE
def generateClassCode(classTable):
    code=""

    for cname in classTable:
        classObj = classTable[cname]

        # Comment code to decipher resulting ami machine code file
        code+=f"# CLASS: {cname}\n"

        for con in classObj.constructors:
            code+=generateConstructorCode(con)

        for meth in classObj.methods:
            code+=generateMethodCode(meth)

    return code

# DONE
def generateConstructorCode(con):
    global am

    code = f"# CONSTRUCTOR: {con.name} {con.id}\n"
    conLabel = f"C_{con.id}"
    code+= f"{conLabel}:\n" 

    # Reset temp reg vals and bindings for new local vars
    am.resetTempRegs()
    am.resetArgRegs()

    # Allocate temp regs for all local vars
    localVars = con.vars
    for var in localVars.values():
        if var.kind == "local":
            am.tempRegTable[var.id] = am.getNewTempReg()

    # Set arg regs correctly based on formal params/args
    am.argRegCount = 1
    formalArgs = con.getFormals()
    for arg in formalArgs:
        am.argRegTable[arg.id] = am.getNewArgReg()
        
    for stmt in con.body.statements:
        code+=generateStatementCode(stmt)

    return code

# DONE
def generateMethodCode(meth):
    global am

    code = f"# METHOD: {meth.name} {meth.id}\n"
    methLabel = f"M_{meth.name}_{meth.id}"
    code += f"{methLabel}:\n" 

    # Reset temp reg vals and bindings for new local vars
    am.resetTempRegs()
    am.resetArgRegs()

    # Allocate temp regs for all local vars
    localVars = meth.vars
    for var in localVars.values():
        if var.kind == "local":
            am.tempRegTable[var.id] = am.getNewTempReg()

    # Set arg regs correctly based on static or not(a0 becomes smth else if instance) and formal params/args
    am.argRegCount = 0 if meth.appli=='static' else 1
    formalArgs = meth.getFormals()
    for arg in formalArgs:
        am.argRegTable[arg.id] = am.getNewArgReg()
        
    for stmt in meth.body.statements:
        code+=generateStatementCode(stmt) 

    return code

# DONE
def generateStatementCode(stmt):
    if type(stmt) == Expr_Statement:
        return generateExprStmtCode(stmt)
    elif type(stmt) == Skip_Statement:
        return ''
    elif type(stmt) == If_Statement:
        return generateIfStmtCode(stmt)
    elif type(stmt) == While_Statement:
        return generateWhileStmtCode(stmt)
    elif type(stmt) == For_Statement:
        return generateForStmtCode(stmt)
    elif type(stmt) == Return_Statement:
        return generateReturnStmtCode(stmt)
    elif type(stmt) == Break_Statement:
        return generateBreakStmtCode(stmt)
    elif type(stmt) == Continue_Statement:
        return generateContinueStmtCode(stmt)
    elif type(stmt) == Block:
        return generateBlockCode(stmt)

# DONE
def generateExprStmtCode(stmt):
    global am

    code = generateExpressionCode(stmt.expr)[0]

    return code

# DONE
ifLabelCounter = 0
def generateIfStmtCode(stmt):
    global am, ifLabelCounter

    elseLabel = f'if_stmt_else_{ifLabelCounter}'
    endLabel = f'if_stmt_end_{ifLabelCounter}'
    ifLabelCounter += 1

    # Check condition
    conditionCode, conditionReturnRegister = generateExpressionCode(stmt.condition)

    code = conditionCode
    code += f'\tbz {conditionReturnRegister}, {elseLabel}\n'

    # Perform then operation
    code += generateStatementCode(stmt.thenBlock)
    code += f'\tjmp {endLabel}\n'

    # Perform else operation
    code += f'{elseLabel}:\n'
    code += generateStatementCode(stmt.elseBlock)

    code += f'{endLabel}:\n'

    return code

# DONE 
whileStatementStartLabelCounter = 0
whileStatementEndLabelCounter = 0
def generateWhileStmtCode(stmt):
    global am, whileStatementStartLabelCounter, whileStatementEndLabelCounter

    startLabel = f"while_start_{whileStatementStartLabelCounter}"
    whileStatementStartLabelCounter += 1
    endLabel = f"while_end_{whileStatementEndLabelCounter}"
    whileStatementEndLabelCounter += 1

    am.enterLoop(startLabel, endLabel)

    # Create label top, evaluate condition, compare and jump to end or continue
    code = f"{startLabel}:\n"

    # Eval condition
    conditionCode, conditionReturnRegister = generateExpressionCode(stmt.condition)
    code += conditionCode
    code += f'\tbz {conditionReturnRegister}, {endLabel}\n'

    # Code body
    code += generateStatementCode(stmt.body)

    code += f"\tjmp {startLabel}\n"
    code += f"{endLabel}:\n"

    am.exitLoop()

    return code

# DONE
forLabelCounter = 0
def generateForStmtCode(stmt):
    global am, forLabelCounter

    startLabel = f'for_start_{forLabelCounter}'
    endLabel = f'for_end_{forLabelCounter}'
    forLabelCounter += 1

    initializerCode, _ = generateExpressionCode(stmt.initializer)
    code = initializerCode

    am.enterLoop(startLabel, endLabel)

    code += f'{startLabel}:\n'

    # Condition Check
    conditionCode, conditionResultRegister = generateExpressionCode(stmt.loopCondition)
    code += conditionCode
    code += f'\tbz {conditionResultRegister}, {endLabel}\n'

    # Body Code
    code += generateStatementCode(stmt.body)

    # Updater
    updaterCode, _ = generateExpressionCode(stmt.updater)
    code += updaterCode
    code += f'\tjmp {startLabel}\n'

    code += f'{endLabel}:\n'

    am.exitLoop()

    return code

# DONE
def generateReturnStmtCode(stmt):
    global am

    code=""

    if stmt.returnExpr:
        exprCode, resultRegister = generateExpressionCode(stmt.returnExpr)
        code+=exprCode
        code+=f"\t# Store return value in {resultRegister} to a0\n"
        code+=f"\tmove a0, {resultRegister}\n"
        code+="\tret\n\n"
    
    return code

# DONE
def generateBreakStmtCode(stmt):
    global am

    breakLabel = am.breakLabel()
    code = f'\tjmp {breakLabel}\n'

    return code

# DONE
def generateContinueStmtCode(stmt):
    global am

    contineLabel = am.continueLabel()
    code = f'\tjmp {contineLabel}\n'

    return code

# DONE
def generateBlockCode(block):    
    code = ''
    for statement in block.statements:
        code += generateStatementCode(statement)

    return code

# DONE
def generateExpressionCode(expr):
    if type(expr) == Assign_Expression:
        return generateAssignExpressionCode(expr)
    elif type(expr) == Const_Expression:
        return generateConstantExpressionCode(expr)
    elif type(expr) == Var_Expression:
        return generateVarExpressionCode(expr)
    elif type(expr) == Unary_Expression:
        return generateUnaryExpressionCode(expr)
    elif type(expr) == Bin_Expression:
        return generateBinExpressionCode(expr)
    elif type(expr) == Auto_Expression:
        return generateAutoExpressionCode(expr)
    elif type(expr) == Field_Access_Expression:
        return generateFieldAccessExpressionCode(expr)
    elif type(expr) == Method_Call_Expression:
        return generateMethodCallExpressionCode(expr)
    elif type(expr) == New_Object_Expression:
        return generateNewObjectExpressionCode(expr)
    elif type(expr) == This_Expression:
        return generateThisExpressionCode(expr)
    elif type(expr) == Super_Expression:
        return generateSuperExpressionCode(expr)
    elif type(expr) == Class_Ref_Expression:
        return generateClassRefExpressionCode(expr)

def generateAssignExpressionCode(expr):
    global am

    code=""

    # Generate code for rhs, then take result and store to lhs.
    lhsCode, lhsresultRegister = generateExpressionCode(expr.lhs)
    rhsCode, rhsresultRegister = generateExpressionCode(expr.rhs)
    

    code+=lhsCode
    code+=rhsCode

    # Might have to do diff sht for regs tht hold heap addr and other sht based on what kinda var we have
    code+=f"\t# Store the rhs value in {rhsresultRegister} into lhs {lhsresultRegister}\n"
    code+=f"\tmove {lhsresultRegister}, {rhsresultRegister}\n"

    return code, lhsresultRegister

# DONE
def generateConstantExpressionCode(expr):
    global am

    code=""
    
    resultRegister = am.getNewTempReg()
    code+=f"\t# Load {expr.type.type} {expr.val} into register {resultRegister}\n"
    if expr.kind == 'LITER_INT':
        code+=f'\tmove_immed_i {resultRegister}, {expr.val}\n'
        return code, resultRegister
    
    elif expr.kind == 'TRUE':
        code+=f'\tmove_immed_i {resultRegister}, 1\n'
        return code, resultRegister
    
    elif expr.kind == 'FALSE':
        code+=f'\tmove_immed_i {resultRegister}, 0\n'
        return code, resultRegister
    
    elif expr.kind == 'LITER_FLOAT':
        code+=f'\tmove_immed_f {resultRegister}, {expr.val}\n'
        return code, resultRegister

    elif expr.kind == 'NULL':
        code+=f'\tmove_immed_i {resultRegister}, 0\n'
        return code, resultRegister

    raise Exception('Constant Type not supported!')

def generateVarExpressionCode(expr):
    global am

    code = ""
    var = expr.var

    # Try catches here for if the var in question is an object instance, or static field
    #     code+= f"# Load {var.name} into register for use\n" 

    # If regular local var
    if var.kind == 'formal':
        reg = am.argRegTable[var.id]
    else:
        reg = am.tempRegTable[var.id]

    return code, reg

unaryExprAfterLabel = 0
unaryExprFalseLabel = 0
def generateUnaryExpressionCode(expr):
    global am, unaryExprAfterLabel, unaryExprFalseLabel

    resultRegister = am.getNewTempReg()
    operandCode, operandResultRegister = generateExpressionCode(expr.operand)

    code = operandCode

    if expr.operand.type.isBool() and expr.operator == '!':
        falseLabel = f'unary_expr_false_{unaryExprFalseLabel}'
        unaryExprFalseLabel += 1
        endLabel = f'unary_expr_end_{unaryExprAfterLabel}'
        unaryExprAfterLabel += 1

        code += f'\tbnz {operandResultRegister}, {falseLabel}\n'

        code += f'\tmove_immed_i {resultRegister}, 1\n'
        code += f'\tj {endLabel}\n'

        code += f'{falseLabel}:\n'
        code += f'\tmove_immed_i {resultRegister}, 0\n'
        code += f'{endLabel}:\n'

        return code, resultRegister
    
    elif expr.operand.type.isInt() and expr.operator == '-':
        code += f'\tmove_immed_i {resultRegister}, -1\n'
        code += f'\timul {resultRegister}, {resultRegister}, {operandResultRegister}\n'

        return code, resultRegister
    
    elif expr.operand.type.isFloat() and expr.operator == '-':
        code += f'\tmove_immed_f {resultRegister}, -1.0\n'
        code += f'\tfmul {resultRegister}, {resultRegister}, {operandResultRegister}\n'

        return code, resultRegister
    
    elif expr.operand.type.isNumber() and expr.operator == '+':
        code += f'\tmove {resultRegister}, {operandResultRegister}\n'

        return code, resultRegister

    raise Exception('Unrecognized unary statement')

binaryExprAfterLabel = 0
binaryExprFalseLabel = 0
def generateBinExpressionCode(expr):
    global am, binaryExprAfterLabel, binaryExprFalseLabel

    # Evaluate both sides of expression and find result
    resultRegister = am.getNewTempReg()

    val1Code, val1resultRegister = generateExpressionCode(expr.val1)
    val2Code, val2resultRegister = generateExpressionCode(expr.val2)

    code = val1Code + val2Code

    if expr.val1.type.isBool() and expr.val2.type.isBool():
        if expr.op == '==':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            # val1 - val2 => Check 0, if yes, change to 1 else change to 0
            code += f'\tisub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n'
            code += f'\tbnz {resultRegister}, {falseLabel}\n'

            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'j {endLabel}\n'

            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister
        
        elif expr.op == '!=':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            # val1 - val2 => Check 0, if yes, change to 1 else change to 0
            code += f'\tisub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n'
            code += f'\tbz {resultRegister}, {falseLabel}\n'

            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'\tj {endLabel}\n'

            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister
        
        elif expr.op == '&&':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            code += f'\tbz {val1resultRegister}, {falseLabel}\n'
            code += f'\tbz {val2resultRegister}, {falseLabel}\n'

            # Expression is true
            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'\tjmp {endLabel}\n'

            # Expression is false
            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister
        
        elif expr.op == '==':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            code += f'\tbnz {val1resultRegister}, {falseLabel}\n'
            code += f'\tbnz {val2resultRegister}, {falseLabel}\n'

            # Expression is true
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'\tjmp {endLabel}\n'

            # Expression is false
            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'{endLabel}:\n'

            return code, resultRegister

    elif expr.val1.type.isInt() and expr.val2.type.isInt():
        if expr.op == '+':
            return code + f'\tiadd {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '-':
            return code + f'\tisub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '/':
            return code + f'\tidiv {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '*':
            return code + f'\timul {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '%':
            return code + f'\timod {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '>':
            return code + f'\tigt {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '>=':
            return code + f'\tigeq {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '<':
            return code + f'\tilt {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '<=':
            return code + f'\tileq {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '==':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            # val1 - val2 => Check 0, if yes, change to 1 else change to 0
            code += f'\tisub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n'
            code += f'\tbnz {resultRegister}, {falseLabel}\n'

            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'\tj {endLabel}\n'

            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister
        
        elif expr.op == '!=':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            # val1 - val2 => Check 0, if yes, change to 1 else change to 0
            code += f'\tisub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n'
            code += f'\tbz {resultRegister}, {falseLabel}\n'

            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'\tj {endLabel}\n'

            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister
    
    elif expr.val1.type.isFloat() or expr.val2.type.isFloat():
        # Convert int to float if exists.
        if expr.val1.type.isInt():
            code += f'\titof {val1resultRegister}, {val1resultRegister}\n'
        elif expr.val2.type.isInt():
            code += f'\titof {val2resultRegister}, {val2resultRegister}\n'
        
        if expr.op == '+':
            return code + f'\tfadd {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '-':
            return code + f'\tfsub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '/':
            return code + f'\tfdiv {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '*':
            return code + f'\tfmul {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '>':
            return code + f'\tfgt {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '>=':
            return code + f'\tfgeq {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '<':
            return code + f'\tflt {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        elif expr.op == '<=':
            return code + f'\tfleq {resultRegister}, {val1resultRegister}, {val2resultRegister}\n', resultRegister
        
        elif expr.op == '==':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            # val1 - val2 => Check 0, if yes, change to 1 else change to 0
            code += f'\tfsub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n'
            code += f'\tbnz {resultRegister}, {falseLabel}\n'

            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'\tj {endLabel}\n'

            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister
        
        elif expr.op == '!=':
            falseLabel = f'binary_expr_false_{binaryExprFalseLabel}'
            binaryExprFalseLabel += 1
            endLabel = f'binary_expr_end_{binaryExprAfterLabel}'
            binaryExprAfterLabel += 1

            # val1 - val2 => Check 0, if yes, change to 1 else change to 0
            code += f'\tfsub {resultRegister}, {val1resultRegister}, {val2resultRegister}\n'
            code += f'\tbz {resultRegister}, {falseLabel}\n'

            code += f'\tmove_immed_i {resultRegister}, 1\n'
            code += f'\tj {endLabel}\n'

            code += f'{falseLabel}:\n'
            code += f'\tmove_immed_i {resultRegister}, 0\n'
            code += f'{endLabel}:\n'

            return code, resultRegister

    raise Exception('Binary Expression operation unknown')

def generateAutoExpressionCode(expr):
    global am

    varCode, varRegister = generateExpressionCode(expr.var)
    tempRegister = am.getNewTempReg()

    code = varCode
    code += f'\tmove_immed_i {tempRegister}, 1\n'

    if expr.op == '+':
        code += f'\tiadd {varRegister}, {varRegister}, {tempRegister}\n'
    else:
        code += f'\tisub {varRegister}, {varRegister}, {tempRegister}\n'

    return code, varRegister

def generateFieldAccessExpressionCode(expr):
    global am

    code = ""
    base = expr.base
    field = expr.fieldObj

    if type(base) == Class_Ref_Expression:
        code+=f'# Get value of static field {field.name}\n'

        #Get the sap offset
        
        sapOffset = am.staticAreaTable[field.id]
        offsetReg = am.getNewTempReg()
        code += f"move_immed_i {offsetReg}, {sapOffset}\n"

        #Load the field value from the sap
        fieldValueReg = am.getNewTempReg()
        code += f"hload {fieldValueReg}, sap, {offsetReg}\n"

        return code, fieldValueReg        
    else:    
        if type(base) == This_Expression or type(base) == Super_Expression:
            code+='# Load the stuff from heap for this or super field access\n'
            objID = expr.objID
            baseClass = base.currClass if type(base)==This_Expression else base.currClass.superClass
        else:
            code+='# Load the stuff from heap for var field access\n'
            # Base is a varexpr tht represents an object in heap
            var = base.var
            objID = var.id
            baseClass = var.type.baseClass
        # Load base addr of obj in heap
        objAddr = "a0" if type(base)== This_Expression or type(base) == Super_Expression else am.heapTable[objID]
        objAddrReg = am.getNewTempReg()
        code+=f"move_immed_i {objAddrReg}, {objAddr}\n"

        # Load field's offset for field access relative to obj addr in heap
        fieldOffset = baseClass.fieldIndex(field)
        fieldOffsetReg = am.getNewTempReg()
        code+=f"move_immed_i {fieldOffsetReg}, {fieldOffset}\n"

        # Load field's val into new temp reg 
        fieldValueReg = am.getNewTempReg()
        code+=f"hload {fieldValueReg}, {objAddrReg}, {fieldOffsetReg}\n"

        return code, fieldValueReg 
    
def generateMethodCallExpressionCode(expr):
    global am

    code = ''
    base = expr.base

    savedArgRegs = list(am.argRegTable.values())
    savedTempRegs = list(am.tempRegTable.values())

    # Save current register context
    for register in savedArgRegs + savedTempRegs:
        code += f'\tsave {register}\n'
    
    # Load in parameters
    argRegCounter = 0 if expr.methodObj.appli == 'static' else 1

    # TODO store reference in a0 if instance method HERE
    if type(base) == This_Expression or type(base) == Super_Expression:
        code+='# Load the stuff from heap for this or super field access\n'
        objID = expr.objID
        baseClass = base.currClass if type(base)==This_Expression else base.currClass.superClass
            # Load base addr of obj in heap
        objAddr = "a0" if type(base)== This_Expression or type(base) == Super_Expression else am.heapTable[objID]
        objAddrReg = am.getNewTempReg()
        code+=f"move {objAddrReg}, {objAddr}\n"
    elif type(base)!=Class_Ref_Expression:
        code+='# Load the stuff from heap for var field access\n'
        # Base is a varexpr tht represents an object in heap
        var = base.var
        objID = var.id
        baseClass = var.type.baseClass
        # Load base addr of obj in heap
        objAddr = "a0" if type(base)== This_Expression or type(base) == Super_Expression else am.heapTable[objID]
        objAddrReg = am.getNewTempReg()
        code+=f"move {objAddrReg}, {objAddr}\n"

    if expr.methodObj.appli!='static':
        code+=f'\tmove a0, {objAddrReg}\n'

    for argument in expr.arguments:
        argCode, argResultRegister = generateExpressionCode(argument)

        code += argCode
        code += f'\tmove a{argRegCounter}, {argResultRegister}\n'

        argRegCounter += 1
    
    # Call the method
    code += f'\tcall M_{expr.methodName}_{expr.methodObj.id}\n'
    
    # Move return val to new register
    returnRegister = am.getNewTempReg()
    code += f'\tmove {returnRegister}, a0\n'

    # Restore register context
    for register in (savedArgRegs + savedTempRegs)[::-1]:
        code += f'\trestore {register}\n'
    
    return code, returnRegister

# DONE
def generateNewObjectExpressionCode(expr):
    global am

    code = ""
    baseClass = expr.base
    args = expr.arguments
    code+= f"\t# Creating new {baseClass.name} object\n"
    am.heapTable[expr.objID] = am.heapTop

    # Count total num of non-static fields to get size of obj in heap
    heapOffset = 0
    current = baseClass
    while current!=None:
        heapOffset+= len(current.instanceFields)
        current = current.superClass

    # Allocating space on heap for new obj
    objRegister = am.getNewTempReg()
    code+=f"\tmove_immed_i {objRegister}, {am.heapTop} # Base addr of obj in {objRegister}\n"
    offsetRegister = am.getNewTempReg()
    code+=f"\tmove_immed_i {offsetRegister}, {heapOffset} # Num of cells/heap offset in {offsetRegister}\n"
    code+=f"\thalloc {objRegister} {offsetRegister} # Allocate {heapOffset} cells for object\n"

    am.heapTop+=heapOffset

    # Save necessary registers if any
    code+=f'\tsave {objRegister}\n'
    for i in range(len(baseClass.constructors[0].vars)):
        if f't{i}'!=objRegister:
            code+=f'\tsave t{i}\n'
    for i in range(len(args)+1):
        code+=f'\tsave a{i}\n'
                
    # Setting args if any
    for i, arg in enumerate(args):
        resultCode, resultReg = generateExpressionCode(arg)
        code+=resultCode
        code+=f'\tmove a{i+1}, {resultReg}\n'
    
    code+=f'\tmove a0, {objRegister} # a0 for constructor always has baseAddr of newObj in heap\n'
    # Call constructor  
    code+=f"\tcall C_{baseClass.constructors[0].id}\n"
    # Restore necessary registers if any
    code+=f'\trestore {objRegister}\n'
    for i in range(len(baseClass.constructors[0].vars)):
        if f't{i}'!=objRegister:
            code+=f'\trestore t{i}\n'
    for i in range(len(args)+1):
        code+=f'\trestore a{i}\n'

    return code, objRegister