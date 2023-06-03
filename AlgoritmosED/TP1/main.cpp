#include <fstream>
#include <iostream>
#include <cstring>
#include <iomanip>
#include <climits>
using namespace std;

const char FILENAME[] = "EtapasRally.txt";
const int MAX_REGS = 100;
const int CENTINELA_ABANDONA = 999;
const int TIPOS_VEHICULOS = 4;
const int CANT_ETAPAS = 5;
const int CANT_MAX_VEHICULOS = 100;

typedef unsigned char BYTE;
typedef unsigned short word; 
typedef char str20[21];
typedef char str15[16];
typedef char str1[2];
typedef char str2[3];
typedef char str3[4];
struct sRally {
    BYTE tVehiculo;
    word nVehiculo;
    str20 nombPiloto,
        nombCopiloto;
    str15 marcaVehiculo;
};

struct sEtapaRally {
    BYTE nEtapa;
    sRally rally;
    word tMinutos;
};

struct sTiempoTotal {
    word tTotal;
    sRally rally;
};

typedef sTiempoTotal arrTiempoTotal[CANT_MAX_VEHICULOS];
typedef sEtapaRally arrEtapasRally[MAX_REGS];
typedef sTiempoTotal arrGanaxTvehi[TIPOS_VEHICULOS];

// Prototipos
void Abrir(ifstream&);

bool LeeretapsRally(ifstream&, sEtapaRally&);
short BusBin(arrEtapasRally, word, word);
bool VerificarEtapaRally(sEtapaRally, arrEtapasRally, short);
void InsertarEnOrden(arrEtapasRally& , word, sEtapaRally, short);
word BusBinCercano(arrEtapasRally , word , word );
void ShiftArray(arrEtapasRally&, word, word);
void ProcesarEtapasRally(ifstream& , arrEtapasRally& , word& );
void EscribirEtapaCorredores(ofstream&, sEtapaRally);
void ListadoCorredores(arrEtapasRally, word);
void EscribirRegistroLargada(ofstream&, sRally);
void OrdxBur(arrEtapasRally , word);
void IntCmb(sEtapaRally& , sEtapaRally&);
void IntCmb(sTiempoTotal& , sTiempoTotal&);
void EscribirCabeceraLargada(ofstream& , BYTE);
void EscribirRegistroLargada(ofstream&, sRally);
void ListadoLargada(arrEtapasRally, word);
void OrdxBurNVehi(arrEtapasRally, word);
void OrdxBurTtotal(arrTiempoTotal, word);
void AgruparxNroVehi(arrEtapasRally, word, arrTiempoTotal&, word&);
void ListadoPuestosFinal(arrEtapasRally, word, arrTiempoTotal&, word&);
void EscribirRegistroPuestoFinal(ofstream&, sTiempoTotal, word);
void EscribirRegistroGanadoresxVehi(ofstream&, sTiempoTotal);
void ProcGanadoresTvehi(arrGanaxTvehi&, word&, arrTiempoTotal, word);
void ListadoGanadoresxTipoVehic(arrTiempoTotal, word);

void Cerrar(ifstream&);
// Fin prototipos


int main() {
  word card, cardTtotales;
  arrEtapasRally EtapasRally;
  arrTiempoTotal TiemposTotales;
  ifstream file;

  Abrir(file);
  ProcesarEtapasRally(file, EtapasRally, card);
  ListadoCorredores(EtapasRally, card);
  ListadoLargada(EtapasRally, card);
  ListadoPuestosFinal(EtapasRally, card, TiemposTotales, cardTtotales);
  ListadoGanadoresxTipoVehic(TiemposTotales, cardTtotales);
  
  Cerrar(file);

  return 0;
}

void Abrir(ifstream& file) {
  file.open(FILENAME);
}

void ProcesarEtapasRally(ifstream& file, arrEtapasRally& arr, word& actual) {
  actual = 0;
    sEtapaRally aux;
    while (LeeretapsRally(file, aux)) {
        short index = BusBin(arr, actual, aux.rally.nVehiculo);
        if (VerificarEtapaRally(aux, arr, index)) {

            if (actual > 0) InsertarEnOrden(arr, actual, aux, index);
            else arr[actual] = aux;

            actual++;
        }
    }
}

bool LeeretapsRally(ifstream& file, sEtapaRally& etapa) {
  sRally rally;

  file >> etapa.nEtapa >> rally.tVehiculo >> rally.nVehiculo;
  file.ignore();
  file.get(rally.nombPiloto, sizeof(rally.nombPiloto));
  file.ignore();
  file.get(rally.nombCopiloto, sizeof(rally.nombCopiloto));
  file.ignore();
  file.get(rally.marcaVehiculo, sizeof(rally.marcaVehiculo));
  file >> etapa.tMinutos;
  etapa.rally = rally;

  return file.good();
}

short BusBin(arrEtapasRally arr, word card, word nVehiculo) {
  short low = 0;
  short high = card - 1;
  short mid;

  if (arr[low].rally.nVehiculo == nVehiculo)
    return low;

  if (arr[high].rally.nVehiculo == nVehiculo)
    return high;

  while (low <= high) {
    mid = (low + high) / 2;
    if (nVehiculo == arr[mid].rally.nVehiculo)
      return mid;
    else if (nVehiculo > arr[mid].rally.nVehiculo)
      low = mid + 1;
    else
      high = mid - 1;
  }

  return -1; // No se encontro el valor

}

bool VerificarEtapaRally(sEtapaRally etapa, arrEtapasRally arr, short index) {
    // Si el numero de vehiculo ingresado existe, tiene que ser el mismo conductor
    // Mismo numero de vehiculo mismo tipo de vehiculo
    // tipo de vehiculo <= 4
    if (etapa.rally.tVehiculo - '0' > 4)
        return false;


    if (index != -1 && 
      (strcmp(arr[index].rally.nombPiloto, etapa.rally.nombPiloto) != 0 
      || arr[index].rally.tVehiculo != etapa.rally.tVehiculo))
        return false;

    return true;
}

void InsertarEnOrden(arrEtapasRally& arr, word card, sEtapaRally etapa, short index) {
    if (index == -1) {
        word cercano = BusBinCercano(arr, card, etapa.rally.nVehiculo);
        cercano = arr[cercano].rally.nVehiculo <= etapa.rally.nVehiculo ? cercano + 1 : cercano;
        ShiftArray(arr, card, cercano);
        arr[cercano] = etapa;
        return;
    }

    index++;
    ShiftArray(arr, card, index);
    arr[index] = etapa;
}

word BusBinCercano(arrEtapasRally arr, word card, word nVehiculo) {
  if (nVehiculo < arr[0].rally.nVehiculo) {
    return 0;
  }

  if (nVehiculo > arr[card - 1].rally.nVehiculo) {
    return card - 1;
  }

  int lo = 0;
  int hi = card - 1;

  while (lo <= hi) {
    int mid = (hi + lo) / 2;

    if (nVehiculo < arr[mid].rally.nVehiculo) 
      hi = mid - 1;
    else if (nVehiculo > arr[mid].rally.nVehiculo) 
      lo = mid + 1;
    else 
      return mid;
  }

  return (arr[lo].rally.nVehiculo - nVehiculo) < (nVehiculo - arr[hi].rally.nVehiculo) ? lo : hi;
}

void ShiftArray(arrEtapasRally& arr, word card, word desde) {
  for (word i = card; i > desde; i--)
    arr[i] = arr[i - 1];
}

void EscribirEtapaCorredores(ofstream& file, sEtapaRally etapa) {

    file << setw(3) << etapa.nEtapa;
    file << setw(8) << etapa.rally.tVehiculo;
    file << setw(26) << etapa.rally.nombPiloto;
    file << setw(20) << etapa.rally.nombCopiloto;
    file << setw(18) << etapa.rally.marcaVehiculo;
    file << setw(8) << etapa.tMinutos << endl;

}

void ListadoCorredores(arrEtapasRally arr, word card) {
  ofstream file;
  file.open("ListadoCorredores.txt");
  file << setw(70) << "Listado de Corredores de Etapas y Tipos de Vehiculos" << endl;
  file << setw(3) << "Etp.";
  file << setw(10) << "TpVehic.";
  file << setw(23) << "Nombre del Piloto   ";
  file << setw(20) << "Nombre del CoPiloto ";
  file << setw(18) << "Marca Vehic.   ";
  file << setw(3) << "Tiempo mins." << endl;
  for (word i = 0; i < card; i++) {
    EscribirEtapaCorredores(file, arr[i]);
  }

  file.close();
}

void IntCmb(sEtapaRally& elem1, sEtapaRally& elem2) {
  sEtapaRally aux = elem1;
  elem1 = elem2;
  elem2 = aux;
} 

void IntCmb(sTiempoTotal& elem1, sTiempoTotal& elem2) {
  sTiempoTotal aux = elem1;
  elem1 = elem2;
  elem2 = aux;
}

void OrdxBur(arrEtapasRally arr, word card) {
  for (word i = 0; i < card - 1; i++) {
    for (word j = i + 1; j < card; j++) {
      if (arr[i].nEtapa > arr[j].nEtapa) IntCmb(arr[i], arr[j]);
      else if (arr[i].nEtapa == arr[j].nEtapa && arr[i].rally.tVehiculo > arr[j].rally.tVehiculo)
        IntCmb(arr[i], arr[j]);
    }
  }
}

void EscribirCabeceraLargada(ofstream& file, BYTE tVehi) {
  file << setw(18) << "Tipo Vehiculo: " << tVehi << endl;
  file << setw(23) << "Nro. Vehic. ";
  file << setw(5) << "Nom. Piloto         ";
  file << setw(5) << "  Nom. CoPiloto         ";
  file << setw(5) << "  Marca Vehic.   " << endl;
}

void EscribirRegistroLargada(ofstream& file, sRally rally) {
  file << setw(18) << rally.nVehiculo;
  file << setw(25) << rally.nombPiloto;
  file << "  " << rally.nombCopiloto;
  file << "    " << rally.marcaVehiculo << endl;
}

void ListadoLargada(arrEtapasRally arr, word card) {
  OrdxBur(arr, card);
  ofstream largada;
  largada.open("ListadoLargada.txt");
  largada << setw(50) << "Listado de Largada" << endl;
  BYTE etapa, tVehic;
  etapa = tVehic = 0;
  for (word i = 0; i < card; i++) {
    if (arr[i].nEtapa != etapa) {
      etapa = arr[i].nEtapa;
      largada << setw(2) << "Nro.Etapa: " << etapa << endl;
      tVehic = 0;
    }

    if (arr[i].rally.tVehiculo != tVehic) {
      tVehic = arr[i].rally.tVehiculo;
      EscribirCabeceraLargada(largada, tVehic);
    }

    EscribirRegistroLargada(largada, arr[i].rally);
  }

  largada.close();
}

void ListadoPuestosFinal(arrEtapasRally etapas, word card, arrTiempoTotal& tTotales, word& cardTtotales) {
    AgruparxNroVehi(etapas, card, tTotales, cardTtotales);
    OrdxBurTtotal(tTotales, cardTtotales);

    ofstream puestosFinales;
    puestosFinales.open("ListadoPuestosFinal.txt");

    puestosFinales << setw(70) << "Listado de Puestos Finales Carrera Rally" << endl;
    puestosFinales << "Puesto T.Vehic. Nro.Vehic. Nom. Piloto          Nom. CoPiloto        ";
    puestosFinales << "Marca Vehic.    Tiempo Total" << endl;

    for (int i = 0; i < cardTtotales; i++)
        EscribirRegistroPuestoFinal(puestosFinales, tTotales[i], i + 1);

    puestosFinales.close();

}

void OrdxBurNVehi(arrEtapasRally arr, word card) {
    for (word i = 0; i < card - 1; i++) {
        for (word j = i + 1; j < card; j++) {
            if (arr[i].rally.nVehiculo > arr[j].rally.nVehiculo) IntCmb(arr[i], arr[j]);
            else if (arr[i].rally.nVehiculo == arr[j].rally.nVehiculo && arr[i].nEtapa > arr[j].nEtapa)
                IntCmb(arr[i], arr[j]);
        }
    }
}

void OrdxBurTtotal(arrTiempoTotal arr, word card) {
    for (word i = 0; i < card - 1; i++) {
        for (word j = i + 1; j < card; j++) {
            if (arr[i].tTotal > arr[j].tTotal)
                IntCmb(arr[i], arr[j]);

        }
    }
}

void AgruparxNroVehi(arrEtapasRally etapas, word card, arrTiempoTotal& arr, word& cardTtotales) {
    OrdxBurNVehi(etapas, card);

    sTiempoTotal aux;

    aux.tTotal = etapas[0].tMinutos;
    aux.rally = etapas[0].rally;
    word nVehi = aux.rally.nVehiculo;

    cardTtotales = 0;

    bool abandonado = aux.tTotal == CENTINELA_ABANDONA;

    for (word i = 1; i < card; i++) {

        if (etapas[i].rally.nVehiculo != nVehi) {
            arr[cardTtotales] = aux;
            cardTtotales++;

            aux.tTotal = 0;
            aux.rally = etapas[i].rally;
            nVehi = aux.rally.nVehiculo;
            abandonado = false;
        }

        if (etapas[i].tMinutos == CENTINELA_ABANDONA)
            abandonado = true;

        if (!abandonado) aux.tTotal += etapas[i].tMinutos;
        else aux.tTotal = CENTINELA_ABANDONA;
    }

    arr[cardTtotales] = aux;
    cardTtotales++;

}

void EscribirRegistroPuestoFinal(ofstream& file, sTiempoTotal tiempo, word puesto) {
    file << setw(3) << puesto;
    file << setw(8) << tiempo.rally.tVehiculo;
    file << setw(10) << tiempo.rally.nVehiculo;
    file << setw(26) << tiempo.rally.nombPiloto;
    file << setw(21) << tiempo.rally.nombCopiloto;
    file << setw(16) << tiempo.rally.marcaVehiculo;
    file << setw(12) << tiempo.tTotal << endl;
}

void ListadoGanadoresxTipoVehic(arrTiempoTotal arr, word card) {
    arrGanaxTvehi ganadoresTvehi;
    word abandonos;
    ProcGanadoresTvehi(ganadoresTvehi, abandonos, arr, card);
    ofstream ganadores;
    ganadores.open("ListadoGanadoresxTipoVehic.txt");

    ganadores << setw(60) << "Listados de Ganadores por tipos de Vehiculos" << endl;
    ganadores << "Tipo Vehic.   Nro. Vehic.   Nombre del Piloto             Tiempo Total" << endl;
    for (word i = 0; i < TIPOS_VEHICULOS; i++)
        EscribirRegistroGanadoresxVehi(ganadores, ganadoresTvehi[i]);

    ganadores << "Cantidad de abandonos: " << abandonos << endl;
    ganadores.close();
}

void ProcGanadoresTvehi(arrGanaxTvehi& ganadoresTvehi, word& abandonados, arrTiempoTotal arrTtotal, word cardTtotal) {

    abandonados = 0;
    for (word i = 0; i < TIPOS_VEHICULOS; i++) {
        sRally rally;
        sTiempoTotal tiempoTotal;
        tiempoTotal.tTotal = USHRT_MAX;
        rally.tVehiculo = i + 1;
        ganadoresTvehi[i] = tiempoTotal;
    }

    for (word i = 0; i < cardTtotal; i++) {
        if (arrTtotal[i].tTotal == CENTINELA_ABANDONA) {
            abandonados++;
            continue;
        }

        word tVehi = arrTtotal[i].rally.tVehiculo - '0';

        if (arrTtotal[i].tTotal < ganadoresTvehi[tVehi - 1].tTotal)
            ganadoresTvehi[tVehi - 1] = arrTtotal[i];
    }
    
}

void EscribirRegistroGanadoresxVehi(ofstream& file, sTiempoTotal tiempo) {
    file << setw(5) << tiempo.rally.tVehiculo;
    file << setw(15) << tiempo.rally.nVehiculo;
    file << setw(28) << tiempo.rally.nombPiloto;
    file << setw(20) << tiempo.tTotal << endl;
}


void Cerrar(ifstream& file) {
  file.close();
}
