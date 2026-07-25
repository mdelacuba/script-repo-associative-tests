#! /bin/bash

# Initially, the input form database (https://forms.gle/tnXxEyTpKEe1xW8i6) was downloaded in CSV format named as form_completo.csv.
# The form_completo.csv file was transposed in LibreOffice and named as form_traspuesto.csv.

# Count columns of each files:
#awk -F "\t" '{print NF}' form_* | sort | uniq -c # 113 cols to form_completo.csv and 306 cols to form_traspuesto.csv.


# Isolate results of Diversity Survey (from non-transposed file) and both Honey-Mumford Questionnaire (from transposed file) in different files:
cut -f1-32,113 form_completo.csv > encuestaD.csv # here includes the final response from the VARK Questionnaire in the last column.
head -n1 form_traspuesto.csv > cuestioH.csv && head -n112 form_traspuesto.csv | tail -n80 >> cuestioH.csv

#------------------------- Diversity Survey --------------------------

# Calculate the frequency of student diversity variables (from col 4..33, include VARK):
for number in {4..33}
do
	cut -f$number encuestaD.csv | grep -vxE "^[[:space:]]*$|." | sort | uniq -c | sed 's/^[ ]*//' | sed "s/ /\t/" | awk -F "\t" '{print $2"\t"$1}' | sed "s/?\t1/?\t/g" | sed "s/:\t1/\t/g" | sed "s/Mercados)\t1/Mercados)\t/g" >> frequenciaD.csv && echo -e "\n" >> frequenciaD.csv
done

##------------------------- Honey-Mumford Questionnaire -------------------

# Add manually the learning style (Pragmatist, Reflector, Activist, and Theorist) to each questionnaire item (by each row) of cuestioH.csv.
# Replace "+" with "1" and replace "-" with "0" manually  in cuestioH.csv
# To sum the percentages of each style within LibreOffice, to each style, copy the values in other spreadsheet and sum in the colum beside the style to obtain the table puntajesH.csv
# Transpose the data in the same table within LibreOffice.

# Convert count or score in baremo results:
paste -d "\t" <(cut -f1 puntajeH.csv) <(echo "Pragmático" && for sample in {1..305}
do
	a=$( grep -w "M$sample" puntajeH.csv | cut -f2 )
	if [[ $a -ge 0 && $a -le 10 ]]; then echo "Muy baja"
	elif [[ $a -ge 11 && $a -le 12 ]]; then echo "Baja"
	elif [[ $a -ge 13 && $a -le 16 ]]; then echo "Moderada"
	elif [[ $a -ge 17 && $a -le 18 ]]; then echo "Alta"
	elif [[ $a -ge 19 && $a -le 20 ]]; then echo "Muy alta"
	else echo "NA"
	fi
done) <(echo "Reflexivo" && for sample in {1..305}
do
	a=$( grep -w "M$sample" puntajeH.csv | cut -f3 )
	if [[ $a -ge 0 && $a -le 12 ]]; then echo "Muy baja"
	elif [[ $a -ge 13 && $a -le 15 ]]; then echo "Baja"
	elif [[ $a -ge 16 && $a -le 17 ]]; then echo "Moderada"
	elif [[ $a -ge 18 && $a -le 19 ]]; then echo "Alta"
	elif [[ $a -eq 20 ]]; then echo "Muy alta"
	else echo "NA"
	fi
done ) <(echo "Activo" && for sample in {1..305}
do
	a=$( grep -w "M$sample" puntajeH.csv | cut -f4 )
	if [[ $a -ge 0 && $a -le 7 ]]; then echo "Muy baja"
	elif [[ $a -ge 8 && $a -le 9 ]]; then echo "Baja"
	elif [[ $a -ge 10 && $a -le 13 ]]; then echo "Moderada"
	elif [[ $a -ge 14 && $a -le 17 ]]; then echo "Alta"
	elif [[ $a -ge 18 && $a -le 20 ]]; then echo "Muy alta"
	else echo "NA"
	fi
done ) <(echo "Teórico" && for sample in {1..305}
do
	a=$( grep -w "M$sample" puntajeH.csv | cut -f5 )
	if [[ $a -ge 0 && $a -le 11 ]]; then echo "Muy baja"
	elif [[ $a -ge 12 && $a -le 13 ]]; then echo "Baja"
	elif [[ $a -ge 14 && $a -le 16 ]]; then echo "Moderada"
	elif [[ $a -ge 17 && $a -le 18 ]]; then echo "Alta"
	elif [[ $a -ge 19 && $a -le 20 ]]; then echo "Muy alta"
	else echo "NA"
	fi
done ) > preferenciaH.txt

# Join Diversity Survey with results from Honey-Mumford Questionnaire:
paste -d "\t" preferenciaH.txt encuestaD.csv > tabla_procesada.csv # AQUÍ MODIFIQUÉ LAS CABECERAS PARA CREAR POSTERIORMENTE LA MATRIZ DE CONTINGENCIA EN R.

# Manually, modify the responses that had the same meaning for the same variable when those variables included the “Other” option and allowed for custom text. This means changing ‘Ns’ and “I don't know” to “Doesn't know”; and “None,” “I don't identify with any religion,” and “None of the above” to “None.”

# Diversity variables codes:
área_estudio	1
nivel_académico	2
etapa_estudio	3
institución_escolar	4
preuniversitario	5
multi_carrera	6
desempeño_anterior	7
trabaja	8
experiencia_laboral	9
edad	10
tecnológico	11
origen_geo	12
territorio	13
movilidad	14
lengua	15
idiomas	16
etnia	17
religión	18
nivel_socioeconómico	19
clase_social	20
func_física	21
func_cognitiva	22
género	23
sexo	24
orientación_sexual	25
motivación	26
objetivo	27
proyección	28
estilo_VARK	29

# For statistical analysis, remove empty rows or cells (those with no response or mark from the participant, since they do not represent a real value, but rather a technical one).

# Extracting p-values from results sheets:

for style in ctg.p ctg.r ctg.a ctg.t
do
	for number in {0..28}
	do
		a=$( grep -wA1 "data:  $style$number" $style*fisher.txt | grep -v "data" | sed "s/p-value = //g" )
		echo -e "$style$number\t$a" >> tabla_fisher.csv
	done
done


for style in ctg.p ctg.r ctg.a ctg.t
do
	for number in {0..28}
	do
		a=$( grep -wA1 "data:  $style$number" $style*chiq.txt | grep -v "data" | cut -f3 -d "," | sed "s/ p-value = //g" )
		echo -e "$style$number\t$a" >> tabla_chiq.csv
	done
done

for number in {0..28}
do
	a=$( grep -wA1 "data:  ctg.vark$number" statsVARK_fisher.txt | grep -v "data" | sed "s/p-value = //g" )
	echo -e "$style$number\t$a"
done

for number in {0..28}
do
	a=$( grep -wA1 "data:  ctg.vark$number" statsVARK_chiq.txt | grep -v "data" | cut -f3 -d "," | sed "s/ p-value = //g" )
	echo -e "$style$number\t$a"
done

# Extract V Cramer values:
grep -w "Cramer" posthoc_vcramer.txt | cut -f2 -d ":"
grep "ctg." posthoc_vcramer.txt | sed 's/> assocstats(//g' | sed 's/)//g'
