import AstNode from "../../Ast/AstNode";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import SymbolTable from "../../SymbolTable/SymbolTable";
import { enumType } from "../../SymbolTable/Type";
import Literal from "../Literal";
import UnaryOperation from "../UnaryOperation";

export default class Unary extends UnaryOperation {
    constructor(exp1: Expression, line: number, column: number) {
        super(exp1, line, column);
    }

    getValue(controller: Controller, symbolTable: SymbolTable): Expression {
        let resExp1 = this.exp1.getValue(controller, symbolTable);

        let typeExp1 = resExp1.type.getTypeName();

        if (typeExp1 == enumType.INTEGER) {
            let value = -(resExp1.value);
            return new Literal(value, enumType.INTEGER);

        } else if (typeExp1 == enumType.DOUBLE) {
            let value = -(resExp1.value);
            return new Literal(value, enumType.DOUBLE);
        }

        return new Literal("Error semantico", enumType.ERROR);
    }
    run(): AstNode {
        throw new Error("Method not implemented.");
    }

}