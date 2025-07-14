# Vincent Zheng
# Aawab Mahmood

from decaf_ast import classTable

# Takes care of storage management, registers, heap, stacks, and printing the program
class abstractMachine:

    def __init__(self) -> None:
        # String holding the program - built in codegen
        self.outputProgram = ""

        # Counter and binding table for argument registers e.g a0, a1
        self.argRegCount = 0
        self.argRegTable = {}

        # Counter and binding table for temporary register e.g t0, t1
        self.tempRegCount = 0
        self.tempRegTable = {}

        # Static area pointer for static field storage area
        self.staticAreaTable = {}

        # Top of heap for new heap offset allocs, and heap addy binding table
        self.heapTop = 0
        self.heapTable = {}

        # Control and Data stacks for return addys and register values
        self.controlStack = []
        self.dataStack = []

        # Stack to keep track of loops
        self.loopStartStack = []
        self.loopEndStack = []

    def resetTempRegs(self):
        self.tempRegCount=0
        self.tempRegTable={}

    def resetArgRegs(self):
        self.argRegCount = 0
        self.argRegTable = {}

    def getNewArgReg(self):
        regName = f"a{self.argRegCount}"
        self.argRegCount+=1
        return regName

    def getNewTempReg(self):
        regName = f"t{self.tempRegCount}"
        self.tempRegCount+=1
        return regName
    
    def enterLoop(self, loopStartLabel, loopEndLabel):
        self.loopStartStack.append(loopStartLabel)
        self.loopEndStack.append(loopEndLabel)
    
    def exitLoop(self):
        self.loopStartStack.pop()
        self.loopEndStack.pop()
    
    def continueLabel(self):
        if len(self.loopStartStack) == 0:
            print('ERROR: Attempted continue outside loop.')
            raise Exception('Attempted continue outside loop.')
        
        return self.loopStartStack[-1]
    
    def breakLabel(self):
        if len(self.loopStartStack) == 0:
            print('ERROR: Attempted continue outside loop.')
            raise Exception('Attempted continue outside loop.')
        
        return self.loopEndStack[-1]

    def fillStaticArea(self):
        # fill static area and offset table with static fields in all classes
        numStaticFields = 0
        sapOffset = 0

        for cname in classTable:
            classObj = classTable[cname]
            numStaticFields+=len(classObj.staticFields)

            for f in classObj.staticFields:
                self.staticAreaTable[f.id] = sapOffset
                sapOffset+=1
        
        # add to self.outputProgram the .static data num static fields sht at the end
        self.outputProgram+=f".static_data {numStaticFields}\n"

    def printProgram(self, fn):
        # Format the filename correctly and create it
        fileName = f"{fn}.ami"
        output = open(fileName, 'w')

        # Write to output        
        output.write(self.outputProgram)
        output.close()
