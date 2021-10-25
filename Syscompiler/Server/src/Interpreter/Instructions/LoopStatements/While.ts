import AstNode from "../../Ast/AstNode";
import SysError from "../../Ast/SysError";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import { Instruction } from "../../Interfaces/Instruction";
import SymbolTable from "../../SymbolTable/SymbolTable";
import { enumType } from "../../SymbolTable/Type";
import Break from "../TransferStatements/Break";
import Continue from "../TransferStatements/Continue";

export default class While implements Instruction {

    public expression: Expression;
    public instructionList: Array<Instruction>;
    public line: number;
    public column: number;

    constructor(expression: Expression, instructionList: Array<Instruction>, line: number, column: number) {
        this.expression = expression;
        this.instructionList = instructionList;
        this.line = line;
        this.column = column;
    }

    execute(controller: Controller, symbolTable: SymbolTable) {

        let temp = controller.isLoopStmt();
        controller.setIsLoopStmt(true);

        if (this.expression.getValue(controller, symbolTable).type.getTypeName() == enumType.BOOLEAN) {
            next:
            while (this.expression.getValue(controller, symbolTable).value == true) {
                let st_Local = new SymbolTable(symbolTable);
                for(let inst of this.instructionList){
                    let ret = inst.execute(controller,st_Local);
                    if (ret instanceof Break) {
                        controller.setIsLoopStmt(true);
                        return ret;
                    }

                    if(ret instanceof Continue){
                        continue next;
                    }
                }
            }
        }else{
            //TODO: REPORTAR ERROR SEMANTICO
            let error = new SysError("Semantico", `Se esperaba expresion booleana. Se econtro ${this.expression.type}`, this.line, this.column);
            controller.addError(error);
            controller.append(` ***ERROR: Se esperaba expresion booleana. Se econtro ${this.expression.type}. En la linea ${this.line} y columna ${this.column}`);
        }
    }

    run(): AstNode {
        throw new Error("Method not implemented.");
    }

}