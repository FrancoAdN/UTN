char* baseAPalabra (short base) {
    if (base == 8) return "Octal";
    return "Hexadecimal";
}

int baseADecimal(char* yytext, short base) {
    long int decimal = 0;
    char* ptr;
    decimal = strtol(yytext, &ptr, base);
    return decimal;
}

char* generarEspacios(short n) {
    if (n <= 0) {
        return NULL;
    }

    char* cadena = (char*)malloc((n + 1) * sizeof(char));
    for (int i = 0; i < n; i++) {
        cadena[i] = ' ';
    }
    cadena[n] = '\0';  
    return cadena;
}