import AstNode from "../../Ast/AstNode";
import SysError from "../../Ast/SysError";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import { Instruction } from "../../Interfaces/Instruction";
import SymbolTable from "../../SymbolTable/SymbolTable";
import Break from "../TransferStatements/Break";
import Case from "./Case";

export default class Switch implements Instruction{

    public expression : Expression;
    public caseList : Array<Case>;
    public defaultCase : Instruction;
    public line : number;
    public column : number;

    constructor(expression : Expression, caseList : Array<Case>, defaultCase : Instruction, line : number,  column : number){
        this.expression = expression;
        this.caseList = caseList;
        this.defaultCase = defaultCase;
        this.line = line;
        this.column = column;
    }

    execute(controller: Controller, st: SymbolTable) {
        let st_local = new SymbolTable(st);

        let breakFlag = false;
        
        let enterCaseFlag = false;
       
        for(let swcase of this.caseList){
            if (this.expression.getValue(controller, st).type.getTypeName() == swcase.expression.getValue(controller,st).type.getTypeName()) {
                if(this.expression.getValue(controller,st).value == swcase.expression.getValue(controller, st).value || enterCaseFlag){
                    enterCaseFlag = true;

                    let res : any = swcase.execute(controller, st_local);
                    if(res instanceof Break){
                        breakFlag = true;
                        return res;
                    }
                }
            }else{
                //error
                //TODO: REPORTAR ERROR SEMANTICO
                let error = new SysError("Semantico", `Incompatibilidad ${swcase.expression.getValue(controller,st).type.getTypeName()} no puede convertirse a ${this.expression.getValue(controller, st).type.getTypeName()} .`, this.line, this.column);
                controller.addError(error);
                controller.append(` ***ERROR: Incompatibilidad ${swcase.expression.getValue(controller,st).type.getTypeName()} no puede convertirse a ${this.expression.getValue(controller, st).type.getTypeName()}. En la linea ${this.line} y columna ${this.column}`);
            }
        }

        if(!breakFlag && this.defaultCase != null){
            
            let res : any = this.defaultCase.execute(controller,st_local);
            if(res instanceof Break){
                breakFlag = true;
                return res;
            }
        }
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }
}