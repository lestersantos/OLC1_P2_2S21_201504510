import AstNode from "../../Ast/AstNode";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import { Instruction } from "../../Interfaces/Instruction";
import SymbolTable from "../../SymbolTable/SymbolTable";
import Break from "../TransferStatements/Break";

export default class Case implements Instruction {

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
        let st_local = new SymbolTable(symbolTable);

        for(let inst of this.instructionList){
            let res : any = inst.execute(controller, st_local);
            //console.log(inst);
            if(res instanceof Break){
                return res
            }
        }
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }



}