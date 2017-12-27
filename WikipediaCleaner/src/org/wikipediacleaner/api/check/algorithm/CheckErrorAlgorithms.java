/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.constants.CWConfiguration;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Helper class for the algorithms.
 */
public final class CheckErrorAlgorithms {

  private static Map<EnumWikipedia, List<CheckErrorAlgorithm>> algorithmsMap =
    new HashMap<EnumWikipedia, List<CheckErrorAlgorithm>>();

  /**
   * Initializes algorithms for a Wikipedia.
   * 
   * @param wikipedia Wikipedia.
   */
  public static synchronized void initializeAlgorithms(EnumWikipedia wikipedia) {
    List<CheckErrorAlgorithm> algorithms = new ArrayList<CheckErrorAlgorithm>(CWConfiguration.MAX_ERROR_NUMBER);
    DecimalFormat errorNumberFormat = new DecimalFormat("000");
    for (int i = 0; i < CWConfiguration.MAX_ERROR_NUMBER; i++) {
      int errorNumber = i + 1;
      CWConfigurationError error = wikipedia.getCWConfiguration().getErrorConfiguration(errorNumber);
      if (error != null) {
        String className = CheckErrorAlgorithm.class.getName() + errorNumberFormat.format(errorNumber);
        CheckErrorAlgorithm algorithm = null;
        try {
          Class algorithmClass = Class.forName(className);
          algorithm = (CheckErrorAlgorithm) algorithmClass.newInstance();
          algorithm.setConfiguration(error);
        } catch (ClassNotFoundException e) {
          // Not found: error not yet available in WikiCleaner.
        } catch (InstantiationException e) {
          System.err.println("InstantiationException for " + className);
        } catch (IllegalAccessException e) {
          System.err.println("IllegalAccessException for " + className);
        } catch (ClassCastException e) {
          System.err.println(
              "Class " + className +
              " doesn't implement " + CheckErrorAlgorithm.class.getName());
        }
        if (algorithm != null) {
          algorithms.add(algorithm);
        }
      }
    }
    algorithmsMap.put(wikipedia, algorithms);
  }

  /**
   * Retrieve all algorithms for a wikipedia.
   * 
   * @param wikipedia Wikipedia.
   * @return All algorithms.
   */
  public static List<CheckErrorAlgorithm> getAlgorithms(EnumWikipedia wikipedia) {
    List<CheckErrorAlgorithm> algorithms = algorithmsMap.get(wikipedia);
    if (algorithms == null) {
      return null;
    }
    List<CheckErrorAlgorithm> tmpAlgorithms = new ArrayList<CheckErrorAlgorithm>();
    for (CheckErrorAlgorithm algorithm : algorithms) {
      if (algorithm != null) {
        tmpAlgorithms.add(algorithm);
      }
    }
    return tmpAlgorithms;
  }

  /**
   * Retrieve an algorithm.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Algorithm requested.
   */
  public static CheckErrorAlgorithm getAlgorithm(
      EnumWikipedia wikipedia, int errorNumber) {
    List<CheckErrorAlgorithm> algorithms = algorithmsMap.get(wikipedia);
    if (algorithms == null) {
      initializeAlgorithms(wikipedia);
      algorithms = algorithmsMap.get(wikipedia);
    }
    if (algorithms == null) {
      return null;
    }
    for (CheckErrorAlgorithm algorithm : algorithms) {
      if (algorithm.getErrorNumber() == errorNumber) {
        return algorithm;
      }
    }
    return null;
  }

  /**
   * @param algorithms List of algorithms.
   * @return List of algorithms numbers.
   */
  public static List<Integer> convertToIntegerList(
      List<CheckErrorAlgorithm> algorithms) {
    List<Integer> result = new ArrayList<>();
    if (algorithms != null) {
      for (CheckErrorAlgorithm algorithm : algorithms) {
        result.add(algorithm.getErrorNumber());
      }
    }
    return result;
  }

  /**
   * @param algorithms List of algorithms numbers.
   * @param wiki Wiki.
   * @return List of algorithms.
   */
  public static List<CheckErrorAlgorithm> convertToAlgorithmList(
      List<Integer> algorithms, EnumWikipedia wiki) {
    List<CheckErrorAlgorithm> result = new ArrayList<>();
    if (algorithms != null) {
      for (Integer algorithm : algorithms) {
        result.add(getAlgorithm(wiki, algorithm.intValue()));
      }
    }
    return result;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Algorithm active ?
   */
  public static boolean isAlgorithmActive(
      EnumWikipedia wikipedia, int errorNumber) {
    CheckErrorAlgorithm algorithm = getAlgorithm(wikipedia, errorNumber);
    if (algorithm == null) {
      return false;
    }
    int priority = algorithm.getPriority();
    return CWConfigurationError.isPriorityActive(priority);
  }
}
