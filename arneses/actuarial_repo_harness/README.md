# Actuarial repository harness

Este arnés está hecho específicamente para **Actuarial-Functions**, un repositorio con scripts R históricos de supervivencia, VaR/MVaR, simulación y otros ejercicios.

## Objetivo

Antes de modernizar código viejo, conviene responder preguntas simples:

1. ¿El script todavía parsea?
2. ¿Depende de rutas locales?
3. ¿Usa funciones interactivas que romperían una ejecución automatizada?
4. ¿Qué paquetes requiere?
5. ¿Qué archivos necesitan atención primero?

Este arnés responde esas preguntas **sin ejecutar los scripts del repositorio**.

## Uso

Desde la raíz del repo:

```bash
Rscript arneses/actuarial_repo_harness/run_harness.R
```

Genera:

```text
arneses/actuarial_repo_harness/_reports/
├── actuarial_harness_report.csv
└── actuarial_harness_report.md
```

## Guardrail importante

El arnés analiza de forma estática. No hace `source()` de los scripts antiguos, por lo que evita disparar rutas locales, ventanas gráficas, lecturas de archivos o código experimental.

## Modo estricto

En `config.R`, cambia:

```r
STRICT_MODE <- TRUE
```

Así el proceso devuelve código de error si encuentra scripts con problemas de sintaxis, lo que permite conectarlo después a CI/CD.
