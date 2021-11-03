import AstNode from "../../Ast/AstNode";
import Controller from "../../Controller";
import Expression from "../../Interfaces/Expression";
import SymbolTable from "../../SymbolTable/SymbolTable";
import { enumType } from "../../SymbolTable/Type";
import Literal from "../Literal";
import Operation from "../Operation";

export default class Subtraction extends Operation {
    constructor(exp1: Expression, exp2: Expression, line: number, column: number) {
        super(exp1, exp2, line, column);
        this.operatorSign = "-";
    }

    getValue(controller: Controller, symbolTable: SymbolTable): Expression {
        let resExp1 = this.exp1.getValue(controller, symbolTable);
        let resExp2 = this.exp2.getValue(controller, symbolTable);

        let typeExp1 = resExp1.type.getTypeName();
        let typeExp2 = resExp2.type.getTypeName();

        if (typeExp1 == enumType.INTEGER) {
            if (typeExp2 == enumType.INTEGER) {
                let value = resExp1.value - resExp2.value;
                return new Literal(value, enumType.INTEGER);
            } if (typeExp2 == enumType.DOUBLE) {
                let value = resExp1.value - resExp2.value;
                return new Literal(value, enumType.DOUBLE);
            } if (typeExp2 == enumType.BOOLEAN) {
                let booleanValue = 1;
                if (resExp2.value == false) {
                    booleanValue = 0;
                }
                let value = resExp1.value - booleanValue;
                return new Literal(value, enumType.INTEGER);
            } if (typeExp2 == enumType.CHAR) {
                let asciiNum = resExp2.value.charCodeAt(0);
                let value = resExp1.value - asciiNum;
                return new Literal(value, enumType.INTEGER);
            } else {
                //TODO: REPORTAR ERROR SEMANTICO
                return new Literal("Error semantico", enumType.ERROR);
            }
        } else if (typeExp1 == enumType.DOUBLE) {
            if (typeExp2 == enumType.INTEGER) {
                let value = resExp1.value - resExp2.value;
                return new Literal(value, enumType.DOUBLE);
            } else if (typeExp2 == enumType.DOUBLE) {
                let value = resExp1.value - resExp2.value;
                return new Literal(value, enumType.DOUBLE);
            } else if (typeExp2 == enumType.BOOLEAN) {
                let booleanValue = 1;
                if (resExp2.value == false) {
                    booleanValue = 0;
                }
                let value = resExp1.value - booleanValue;
                return new Literal(value, enumType.DOUBLE);
            } else if (typeExp2 == enumType.CHAR) {
                let asciiNum = resExp2.value.charCodeAt(0);
                let value = resExp1.value - asciiNum;
                return new Literal(value, enumType.DOUBLE);
            } else {
                //TODO: REPORTAR ERROR SEMANTICO
                return new Literal("Error semantico", enumType.ERROR);
            }
        } else if (typeExp1 == enumType.BOOLEAN) {
            let booleanValue = 1;
            if (resExp1.value == false) {
                booleanValue = 0;
            }

            if (typeExp2 == enumType.INTEGER) {
                let value = booleanValue - resExp2.value;
                return new Literal(value, enumType.INTEGER);
            } else if (typeExp2 == enumType.DOUBLE) {
                let value = booleanValue - resExp2.value;
                return new Literal(value, enumType.DOUBLE);
            } else {
                //TODO: REPORTAR ERROR SEMANTICO
                return new Literal("Error semantico", enumType.ERROR);
            }
        } else if (typeExp1 == enumType.CHAR) {
            let asciiNum = resExp1.value.charCodeAt(0);
            if (typeExp2 == enumType.INTEGER) {
                let value = asciiNum - resExp2.value;
                return new Literal(value, enumType.INTEGER);
            } else if (typeExp2 == enumType.DOUBLE) {
                let value = asciiNum - resExp2.value;
                return new Literal(value, enumType.DOUBLE);
            } else {
                //TODO: REPORTAR ERROR SEMANTICO
                return new Literal("Error semantico", enumType.ERROR);
            }
        }

        return new Literal("Error semantico", enumType.ERROR);
    }

}