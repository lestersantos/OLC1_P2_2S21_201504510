import express = require('express');
import Ast from '../../src/Interpreter/Ast/Ast';
import Controller from '../../src/Interpreter/Controller';
import SymbolTable from '../../src/Interpreter/SymbolTable/SymbolTable';

var jisonParser = require('../../src/Analyzer/interpreter').parser;

const router = express.Router();

const fs = require('fs');

const dataBuffer = readFile();

testGrammar(dataBuffer.toString());

router.get('/', (req, res) => {
    res.send("Hello Express rom routes.ts file!");
})

router.get('/help', (req, res) => {
    res.send('Help page');
})

router.post('/evaluate', (req, res) => {
    try {
        const { input } = req.body;

        const input2 = req.body.input;

        let instructionsArray = jisonParser.parse(input2);

        let response = "";

        for (let evaluate of instructionsArray) {
            console.log(`El valor de la expresion es:  ${evaluate.expresion}`);
            response += `El valor de la expresion es:  ${evaluate.expresion} `;
        }
        res.status(200).json({ resultado: response });
    } catch (error) {
        console.log(error);
        res.status(500).json({ resultado: "Se ha producido un error al analizar la cadena" });
    }
})

function readFile() {
    const text = fs.readFileSync('\app\\test.txt');
    return text;
}

function testGrammar(input: string) {
    
    const var1 = '"';
    console.log(var1);

    console.log(input);

    let ast : Ast = jisonParser.parse(input);

    let controller = new Controller();
    let global_TableSymbol = new SymbolTable(null);

    ast.execute(controller, global_TableSymbol);

    console.log(controller.consoleMsg);
    console.log("Analisis finalizado");
}


module.exports = router;