import AstNode from "../../Ast/AstNode";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import SymbolTable from "../../SymbolTable/SymbolTable";
import Type, { enumType } from "../../SymbolTable/Type";
import Literal from "../Literal";
import Operation from "../Operation";
import UnaryOperation from "../UnaryOperation";

export default class Not extends UnaryOperation {

    constructor(exp1: Expression, line: number, column: number) {
        super(exp1, line, column);
    }

    getType(controller: Controller, symbolTable: SymbolTable): Type {
        return this.type;
    }
    getValue(controller: Controller, symbolTable: SymbolTable): Expression {

        let resExp1 = this.exp1.getValue(controller, symbolTable);
        let typeExp1 = resExp1.type.getTypeName();

        if (typeExp1 == enumType.BOOLEAN) {
            let value = !resExp1.value;
            return new Literal(value, enumType.BOOLEAN);
        }

        return new Literal("Error semantico", enumType.ERROR);
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }

}