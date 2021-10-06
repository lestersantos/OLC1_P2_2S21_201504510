import express = require('express');
import { text } from 'stream/consumers';
var parser = require('../../src/Analyzer/interpreter').parser;

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

        let instructionsArray = parser.parse(input2);

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

    console.log(input);

    let instructionsArray = parser.parse(input);

    for (let evaluate of instructionsArray) {
        console.log(`El valor de la expresion es:  ${evaluate.expresion}`);
    }

    console.log("Analisis finalizado");
}


module.exports = router;