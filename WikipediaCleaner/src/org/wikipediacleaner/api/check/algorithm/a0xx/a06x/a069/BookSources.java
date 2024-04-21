/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2024  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a069;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

/** Names of special page BookSources depending on the wiki */
final class BookSources {

  public static final Map<String, Pair<Set<String>, Set<String>>> MAP = createMap();
  
  private BookSources() {
    // Do nothing: utility class
  }

  private static Map<String, Pair<Set<String>, Set<String>>> createMap() {
    final Map<String, Pair<Set<String>, Set<String>>> map = new HashMap<>();
    map.put("cs", createCS());
    map.put("de", createDE());
    map.put("en", createEN());
    map.put("fr", createFR());
    map.put("it", createIT());
    map.put("nl", createNL());
    return Collections.unmodifiableMap(map);
  }
  
  private static Pair<Set<String>, Set<String>> createCS() {
    final Set<String> namespaceNames = new HashSet<>();
    namespaceNames.add("Speciální");
    final Set<String> pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("Zdroje knih");
    pageNames.add("KnižnéZdroje");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createDE() {
    final Set<String> namespaceNames = new HashSet<>();
    namespaceNames.add("Spezial");
    final Set<String> pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("ISBN Suche");
    pageNames.add("ISBN-Suche");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createEN() {
    final Set<String> namespaceNames = new HashSet<>();
    namespaceNames.add("Special");
    final Set<String> pageNames = new HashSet<>();
    pageNames.add("BookSources");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createFR() {
    final Set<String> namespaceNames = new HashSet<>();
    namespaceNames.add("Spécial");
    namespaceNames.add("Sp%C3%A9cial");
    final Set<String> pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("Ouvrages de référence");
    pageNames.add("Ouvrages de reference");
    pageNames.add("Ouvragesderéférence");
    pageNames.add("Ouvragesdereference");
    pageNames.add("Recherche ISBN");
    pageNames.add("Recherche isbn");
    pageNames.add("RechercheISBN");
    pageNames.add("Rechercheisbn");
    pageNames.add("Ouvrages%20de%20r%C3%A9f%C3%A9rence");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createIT() {
    final Set<String> namespaceNames = new HashSet<>();
    namespaceNames.add("Speciale");
    final Set<String> pageNames = new HashSet<>();
    pageNames.add("BookSources");
    pageNames.add("RicercaISBN");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createNL() {
    final Set<String> namespaceNames = new HashSet<>();
    namespaceNames.add("Speciaal");
    final Set<String> pageNames = new HashSet<>();
    pageNames.add("Boekbronnen");
    pageNames.add("Boekinformatie");
    pageNames.add("BookSources");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }
}
