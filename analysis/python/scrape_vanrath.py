#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 23 10:37:46 2019

@author: owen
"""

import os
import re
import json
import requests

from bson import json_util
from bs4 import BeautifulSoup
from string import punctuation



DATA_DIR_PATH = os.getcwd() + '/data'


PROGRAMMING_LANGUAGES = [
    'python',
    'linux',
    'java',
    'scala',
    'clojure'
    'html',
    'golang',
    'javascript',
    'ruby',
    'rust',
    'erlang',
    'objective-c',
    'objective c',
    'c#',
    'c++',
    'swift',
    'kotlin',
    'typescript',
    'sql']



BASIC_REQUEST = {
    "baseURL": "https://vanrath.com",
    "userAgent": "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:24.0) Gecko/20100101 Firefox/24.0"
}



def get_job_search_page():
    search_url = '{}{}'.format(
        BASIC_REQUEST['baseURL'], 
        '/it-jobs/?jss=75&jsc=0&jsl=0&jobs_per_page=1000')
    request_page = requests.get(
        url=search_url,
        headers={'User-Agent': BASIC_REQUEST['userAgent']})
    if request_page.status_code == 200:
        search_page_soup = BeautifulSoup(request_page.content, "lxml")
        return search_page_soup


def parse_job_details(url):
    request_page = requests.get(
        url=url,
        headers={'User-Agent': BASIC_REQUEST['userAgent']})
    detail_page_soup = BeautifulSoup(request_page.content, "lxml")
    description = detail_page_soup.find("div", {"class": "s-cms s-job-details"}).get_text().strip()
    keywords = detail_page_soup.find("p", {"class": "u-bold theme__color u-m-b3"}).get_text().strip().split(",")
    return {'description': description, 'keywords': keywords}


def parse_all_listings(search_page_soup):
    listings = search_page_soup.find("div", {"class": "c-listings u-m-y2 t-it"})
    all_grids = listings.find_all("div", {"class": "o-grid__item u-width-3/4 u-width-full@s u-m-b2@s"})
    full_ = listings.find_all("a", {"class": "c-listings__item t-it"})
    job_specs = []
    for i in range(len(all_grids)):
        print("{}/{}: Collecting information on {}".format(i, len(all_grids)-1, all_grids[i].find('h3').get_text()))
        job_specs.append({
            'role': all_grids[i].find('h3').get_text(),
            'location': all_grids[i].find("p", {"class": "u-bold u-color-grey2"}).get_text(),
            'salaryRange': all_grids[i].find("p", {"class": "u-light u-text-small"}).get_text(),
            'contractType': all_grids[i].find("p", {"class": "u-text-meta u-uppercase u-color-grey2"}).get_text(),
            'hyperlink': full_[i].attrs['href'],
            'details': parse_job_details(url=full_[i].attrs['href'])})
    return job_specs


def count_languages(job_specs):
    language_counter = {}
    for role in job_specs:
        if 'Go ' in role['details']['description']:
            if 'Go' in language_counter.keys():
                language_counter['Go'] += 1
            else:
                language_counter.update({'Go': 1})
        desc_lower = role['details']['description'].lower()
        for language in PROGRAMMING_LANGUAGES:
            if language in desc_lower:
                if language in language_counter.keys():
                    language_counter[language] += 1
                else:
                    language_counter.update({language: 1})
    return language_counter


search_page = get_job_search_page()
all_job_listings = parse_all_listings(
    search_page_soup=search_page)
language_counter = count_languages(
    job_specs=all_job_listings)


min_wages = []; max_wages = []
for role in all_job_listings:
    str = re.sub("£", "", role['salaryRange'])
    str = str.translate(str.maketrans('', '', punctuation))
    wage_list = [int(s) for s in str.split() if s.isdigit()]
    role['wage'] = wage_list
    if role['contractType'] == 'Permanent':
        if len(wage_list) > 1:
            max_wages.append(wage_list[len(wage_list)-1])
        if len(wage_list) >= 1:
            min_wages.append(wage_list[0])

with open(DATA_DIR_PATH + '/jobSpecs.json', 'w') as fp:
    json.dump(all_job_listings, fp)
    