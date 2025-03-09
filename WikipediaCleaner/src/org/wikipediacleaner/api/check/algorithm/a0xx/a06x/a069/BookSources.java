/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2024  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a069;

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
      return Map.ofEntries(
              Map.entry("cs", createCS()),
              Map.entry("de", createDE()),
              Map.entry("en", createEN()),
              Map.entry("es", createES()),
              Map.entry("fr", createFR()),
              Map.entry("it", createIT()),
              Map.entry("nl", createNL()),
              Map.entry("ru", createRU()));
  }
  
  private static Pair<Set<String>, Set<String>> createCS() {
    final Set<String> namespaceNames = Set.of("Speciální");
    final Set<String> pageNames = Set.of("BookSources", "Zdroje knih", "KnižnéZdroje");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createDE() {
    final Set<String> namespaceNames = Set.of("Spezial");
    final Set<String> pageNames = Set.of("BookSources", "ISBN Suche", "ISBN-Suche");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createEN() {
    final Set<String> namespaceNames = Set.of("Special");
    final Set<String> pageNames = Set.of("BookSources");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createES() {
    final Set<String> namespaceNames = Set.of("Especial");
    final Set<String> pageNames = Set.of("BookSources", "FuentesDeLibros");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createFR() {
    final Set<String> namespaceNames = Set.of("Spécial", "Sp%C3%A9cial");
    final Set<String> pageNames = Set.of(
            "BookSources",
            "Ouvrages de référence",
            "Ouvrages de reference",
            "Ouvragesderéférence",
            "Ouvragesdereference",
            "Recherche ISBN",
            "Recherche isbn",
            "RechercheISBN",
            "Rechercheisbn",
            "Ouvrages de r%C3%A9f%C3%A9rence",
            "Ouvrages%20de%20r%C3%A9f%C3%A9rence");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createIT() {
    final Set<String> namespaceNames = Set.of("Speciale");
    final Set<String> pageNames = Set.of("BookSources", "RicercaISBN");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createNL() {
    final Set<String> namespaceNames = Set.of("Speciaal");
    final Set<String> pageNames = Set.of("Boekbronnen", "Boekinformatie", "BookSources");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }

  private static Pair<Set<String>, Set<String>> createRU() {
    final Set<String> namespaceNames = Set.of("Служебная");
    final Set<String> pageNames = Set.of("BookSources", "Источники книг");
    return new ImmutablePair<>(namespaceNames, pageNames);
  }
}
