import re

import requests
import pandas as pd
import numpy as np
from bs4 import BeautifulSoup

from regions_map import *


DEMOSCOPE_URL = "https://www.demoscope.ru/weekly/ssp/rus_pop_reg.php"
ROSSTAT_URL = "https://rosstat.gov.ru/bgd/regl/b12_13/IssWWW.exe/Stg/d1/04-03.htm"
STATBASE_URL = "https://statbase.ru/data/rus-population-by-region-national-stat/"


df = pd.read_csv("final_data.csv", sep=",", encoding="utf-8")
df['Регион'] = df['Регион'].str.strip()

# Добавляем геоданные (широта и расстояние до Москвы)
df['latitude'] = df['Регион'].map(
    lambda x: coord_dist_map.get(x, (None, None))[0]
)
df['distance_to_moscow_km'] = df['Регион'].map(
    lambda x: coord_dist_map.get(x, (None, None))[1]
)

# ДАННЫЕ ЗА 2000 И 2005 (DEMOSCOPE) 
tables = pd.read_html(DEMOSCOPE_URL, encoding="windows-1251")
pop_df = tables[0]

pop_df = pop_df.iloc[4:].copy()                                      # убираем шапку сайта
pop_df = pop_df.drop(pop_df.columns[[1, 2, 5, 6, 7, 8]], axis=1)    # оставляем только Регион + 2000 + 2005
pop_df = pop_df.reset_index(drop=True)

# Первая строка — заголовок
pop_df.columns = pop_df.iloc[0]
pop_df = pop_df.iloc[1:].reset_index(drop=True)

pop_df = pop_df.rename(columns={
    pop_df.columns[0]: 'Регион',
    pop_df.columns[1]: 'pop_2000',
    pop_df.columns[2]: 'pop_2005'
})

# Убираем строку с РФ
pop_df = pop_df[~pop_df['Регион'].str.contains('Российская Феде', na=False)].copy()

# Приводим население к числам (в тысячах человек)
pop_df['pop_2000'] = pd.to_numeric(pop_df['pop_2000'], errors='coerce')
pop_df['pop_2005'] = pd.to_numeric(pop_df['pop_2005'], errors='coerce')

pop_df['key'] = pop_df['Регион'].map(region_map_demoscope)

pop_2000_dict = pop_df.set_index('key')['pop_2000'].to_dict()
pop_2005_dict = pop_df.set_index('key')['pop_2005'].to_dict()

df['Population'] = np.nan

mask_2000 = (df['Год'] == 2000)
df.loc[mask_2000, 'Population'] = df.loc[mask_2000, 'Регион'].map(pop_2000_dict)

mask_2005 = (df['Год'] == 2005)
df.loc[mask_2005, 'Population'] = df.loc[mask_2005, 'Регион'].map(pop_2005_dict)

# ДАННЫЕ ЗА 2001, 2006–2012 (ROSSTAT) 
response = requests.get(ROSSTAT_URL, verify=False)
response.encoding = 'windows-1251'

soup = BeautifulSoup(response.text, 'html.parser')
raw_text = soup.get_text(separator='\n')
lines = [line.strip() for line in raw_text.split('\n') if line.strip()]

# Пропускаем шапку
i = 0
while i < len(lines) and lines[i] != 'Все население':
    i += 1
if i < len(lines):
    i += 1

years = [1990, 1996, 2001, 2006, 2007, 2008, 2009, 2010, 2011, 2012]
data = []


def has_cyrillic(text: str) -> bool:
    return bool(re.search(r'[а-яё]', text, re.IGNORECASE))


while i < len(lines):
    if has_cyrillic(lines[i]):
        name_parts = []
        while i < len(lines) and has_cyrillic(lines[i]):
            name_parts.append(lines[i])
            i += 1
        name = ' '.join(name_parts).strip()

        # Следующие 10 строк — числа
        values = []
        for _ in range(10):
            if i < len(lines):
                num_str = lines[i].replace(' ', '').strip()
                if num_str.isdigit():
                    values.append(int(num_str))
                i += 1

        if len(values) == 10:
            row = {'subject': name}
            row.update(dict(zip(years, values)))
            data.append(row)
    else:
        i += 1

pop_df = pd.DataFrame(data)
pop_df = pop_df.iloc[1:].copy()          # убираем строку "РФ"
pop_df = pop_df.drop(pop_df.columns[[1, 2]], axis=1)  # убираем 1990 и 1996

# Берём только регионы (до строк "Городское население")
pop_df = pop_df.head(91)

pop_df = pop_df[~pop_df['subject'].str.contains('Городское население|Сельское население', na=False)].copy()

pop_df['key'] = pop_df['subject'].map(region_map_rosstat)

# Особые случаи для Тюменской области и автономных округов
pop_df.loc[pop_df['subject'] == 'Тюменская область', 'key'] = 'Тюменская область'
pop_df.loc[pop_df['subject'].str.contains('Ханты-Мансийский', na=False), 'key'] = 'Ханты-Мансийский ...'

# Создаём словари по годам
pop_dict = {}
for year in [2001, 2006, 2007, 2008, 2009, 2010, 2011, 2012]:
    pop_dict[year] = pop_df.set_index('key')[year].to_dict()

for year, p_dict in pop_dict.items():
    mask = (df['Год'] == year)
    df.loc[mask, 'Population'] = df.loc[mask, 'Регион'].map(p_dict)

# Добавляем федеральный округ
df['district'] = df['Регион'].map(district_map)

df.to_csv("extended_final_data.csv", index=False, encoding="utf-8-sig")

# ЗАПОЛНЕНИЕ 2002–2004 ИЗ data.txt (statbase)
data_dict = {}

with open('data.txt', encoding='utf-8') as f:
    lines = f.readlines()

for line in lines[1:]:
    parts = line.strip().split('\t')
    if len(parts) < 5:
        continue

    region = parts[0].strip()
    values = parts[1:]
    years = [2005, 2004, 2003, 2002]

    for year, value in zip(years, values):
        if year not in [2002, 2003, 2004]:
            continue
        value = int(value.replace(' ', ''))
        mapped_region = region_map_statbase.get(region, region)
        data_dict[(year, mapped_region)] = value

df = pd.read_csv('extended_final_data.csv')

for i, row in df.iterrows():
    year = row['Год']
    region = row['Регион']
    if pd.isna(row['Population']):
        key = (year, region)
        if key in data_dict:
            df.at[i, 'Population'] = data_dict[key] // 1000

df.to_csv('result.csv', index=False)

print("Готово! Файл сохранён как result.csv")