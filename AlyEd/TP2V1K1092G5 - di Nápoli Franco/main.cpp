#include <fstream>
#include <iostream>
#include <iomanip>
using namespace std;

const char FILENAME[] = "EtapasRally.dat";
const int MAX_REGS = 100;
const int CENTINELA_ABANDONA = 999;
const int TIPOS_VEHICULOS = 4;
const int CANT_ETAPAS = 5;

typedef struct sNodo *tLista;
typedef short tvTmpEtp[CANT_ETAPAS];

typedef unsigned char BYTE;
typedef unsigned short word;
typedef char str20[21];
typedef char str15[16];
typedef char str1[2];
typedef char str2[3];
typedef char str3[4];

struct sRally
{
    BYTE nEtapa, tVehi;
    word nVehi;
    str20 nPiloto,
        nCopiloto;
    str15 marca;
    word tMinutos;
};

struct tInfo
{
    word nVehi;
    int referencia;
    tvTmpEtp tiempos;
};

struct sNodo
{
    tInfo rInfo;
    tLista sgte;
};

/* Prototipos     */
void Abrir(ifstream &);
void Cerrar(ifstream &);

tLista ExisteNodo(tLista &, tLista &, word);

void InsertaEnLugar(tLista, tInfo);
void InsertaInicio(tLista &, tInfo);
void CrearNodo(tLista &, tLista &, tInfo);

void ProcEtapasRally(ifstream &, tLista &);

void SacarPrimerNodo(tLista &, tInfo &);
void ListadoCorredores(ifstream &, tLista &);

/* Fin prototipos */

// bloque principal
int main()
{
    tLista ListaRally = NULL;
    ifstream EtapasRally;

    Abrir(EtapasRally);
    ProcEtapasRally(EtapasRally, ListaRally);
    ListadoCorredores(EtapasRally, ListaRally);
    Cerrar(EtapasRally);
    return 0;
}

void Abrir(ifstream &file)
{
    file.open(FILENAME, ios::binary);
}

void Cerrar(ifstream &file)
{
    file.close();
}

bool RallyAbandona(tvTmpEtp tmpEtp)
{
    bool abandono = false;
    for (word i = 0; i < CANT_ETAPAS; i++)
    {
        if (tmpEtp[i] == CENTINELA_ABANDONA)
        {
            abandono = true;
            break;
        }
    }

    return abandono;
}

void ProcEtapasRally(ifstream &EtapasRally, tLista &Lista)
{
    sRally rEtapa;
    tLista pAnt, pNodoVehi;
    tInfo rInfo;
    int referencia;

    while (EtapasRally.read((char *)&rEtapa, sizeof(rEtapa)))
    {
        pNodoVehi = ExisteNodo(Lista, pAnt, rEtapa.nVehi);
        short etapa = (rEtapa.nEtapa - '0') - 1;
        if (!pNodoVehi)
        {
            referencia = (int)EtapasRally.tellg() - ((int)sizeof(rEtapa));
            rInfo.nVehi = rEtapa.nVehi;
            rInfo.referencia = referencia;
            rInfo.tiempos[etapa] = rEtapa.tMinutos;
            CrearNodo(Lista, pAnt, rInfo);
        }
        else
        {
            if (!RallyAbandona(pNodoVehi->rInfo.tiempos))
                pNodoVehi->rInfo.tiempos[etapa] = rEtapa.tMinutos;
            else
                pNodoVehi->rInfo.tiempos[etapa] = CENTINELA_ABANDONA;
        }
    }

    Cerrar(EtapasRally);
}

void CrearNodo(tLista &Lista, tLista &pAnt, tInfo valor)
{
    tLista pNodo;

    if (!pAnt)
        InsertaInicio(Lista, valor);
    else
        InsertaEnLugar(pAnt, valor);
} // CrearNodo

void InsertaEnLugar(tLista pAnt, tInfo valor)
{
    tLista pNodo;

    pNodo = new sNodo;
    pNodo->rInfo = valor;
    pNodo->sgte = pAnt->sgte;
    pAnt->sgte = pNodo;
}

void InsertaInicio(tLista &Lista, tInfo valor)
{
    tLista pNodo = new sNodo;

    pNodo->rInfo = valor;
    pNodo->sgte = Lista;
    Lista = pNodo;

} // InsertaInicio

tLista iif(bool exprLog, tLista ptr1, tLista ptr2)
{
    if (exprLog)
        return ptr1;
    return ptr2;
} // iif

tLista ExisteNodo(tLista &Lista, tLista &pAnt, word nVehi)
{
    tLista pAct = Lista;

    pAnt = NULL;
    while (pAct && nVehi > pAct->rInfo.nVehi)
    {
        pAnt = pAct;
        pAct = pAct->sgte;
    }
    return iif(pAct && nVehi == pAct->rInfo.nVehi, pAct, NULL);
} // ExisteNodo

void SacarPrimerNodo(tLista &Lista, tInfo &valor)
{
    tLista pElim = Lista;

    valor = Lista->rInfo;
    Lista = Lista->sgte;
    delete pElim;
}

void BuscarPorReferencia(int referencia, sRally &rEtapa, ifstream &EtapasRally)
{
    EtapasRally.seekg(referencia, ios::beg);
    EtapasRally.read((char *)&rEtapa, sizeof(rEtapa));
}

void EscribirCorredor(ofstream &file, sRally etapa, tvTmpEtp tmpEtp)
{
    file << setw(3) << etapa.tVehi;
    file << setw(12) << etapa.nVehi;
    file << setw(25) << etapa.nPiloto;
    file << setw(20) << etapa.nCopiloto;
    file << setw(18) << etapa.marca;
    file << setw(8);
    for (word i = 0; i < CANT_ETAPAS; i++)
    {
        file << tmpEtp[i] << ' ';
    }
    file << endl;
}

void ListadoCorredores(ifstream &EtapasRally, tLista &Lista)
{

    Abrir(EtapasRally);
    ofstream ListadoCorredores;
    ListadoCorredores.open("ListadoNroVehicRally.Txt");
    tInfo rInfo;
    sRally rEtapa;
    ListadoCorredores << setw(85) << "Listado de Corredores de tiempos por etapas ord. x Nro. Vehiculo" << endl
                      << endl;
    ListadoCorredores << setw(3) << "T.Vehic";
    ListadoCorredores << setw(12) << "Nro.Vehic.";
    ListadoCorredores << setw(12) << "Nom. Piloto";
    ListadoCorredores << setw(22) << "Nom. CoPiloto";
    ListadoCorredores << setw(22) << "Marca Vehic.";
    ListadoCorredores << setw(25) << "Tiempo en minutos" << endl
                      << endl;

    word abandonos = 0;
    while (Lista)
    {
        SacarPrimerNodo(Lista, rInfo);
        BuscarPorReferencia(rInfo.referencia, rEtapa, EtapasRally);
        if (rInfo.tiempos[0] != CENTINELA_ABANDONA)
            EscribirCorredor(ListadoCorredores, rEtapa, rInfo.tiempos);
        else
            abandonos++;
    }
    ListadoCorredores << "Cant. de abandonos: " << abandonos << endl;

    ListadoCorredores.close();
}
