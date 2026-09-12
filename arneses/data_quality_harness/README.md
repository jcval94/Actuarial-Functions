# Data quality harness

Arnés genérico para poner una barrera de calidad antes de analizar un CSV.

No está atado a Actuarial-Functions: puede reutilizarse en otros proyectos.

## Uso

```bash
Rscript arneses/data_quality_harness/run_data_quality.R packs.csv
```

También puedes indicar una carpeta de salida:

```bash
Rscript arneses/data_quality_harness/run_data_quality.R data.csv reports/data
```

## Qué valida

- cantidad de filas y columnas;
- duplicados;
- porcentaje de nulos por columna;
- tipo inferido;
- cardinalidad;
- resumen de variables numéricas.

## Guardrails configurables

Por defecto marca `CHECK` si:

- una columna supera 20% de nulos; o
- los duplicados superan 5% de las filas.

Puedes cambiar los umbrales sin modificar código:

```bash
MAX_MISSING_PCT=10 MAX_DUPLICATE_PCT=1 \
Rscript arneses/data_quality_harness/run_data_quality.R packs.csv
```

Para convertir esos checks en un fallo automatizable:

```bash
STRICT_MODE=true Rscript arneses/data_quality_harness/run_data_quality.R packs.csv
```

## Por qué esto es un arnés

Una función que calcula nulos sería un *skill*. Aquí hay un flujo completo: entrada → perfilado → reglas → decisión PASS/CHECK → artefactos de salida → código de salida opcional para automatización.
