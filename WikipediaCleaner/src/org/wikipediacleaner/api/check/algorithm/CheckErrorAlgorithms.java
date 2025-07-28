/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.configuration.CWConfiguration;
import org.wikipediacleaner.api.configuration.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Helper class for the algorithms.
 */
public final class CheckErrorAlgorithms {

  private static final Map<EnumWikipedia, List<CheckErrorAlgorithm>> algorithmsMap = new HashMap<>();

  /**
   * Initializes algorithms for a Wiki.
   * 
   * @param wiki Wiki.
   */
  public static synchronized void initializeAlgorithms(EnumWikipedia wiki) {
    List<CheckErrorAlgorithm> algorithms = new ArrayList<>(CWConfiguration.MAX_ERROR_NUMBER);
    for (int i = 0; i < CWConfiguration.MAX_ERROR_NUMBER; i++) {
      int errorNumber = i + 1;
      CWConfigurationError error = wiki.getCWConfiguration().getErrorConfiguration(errorNumber);
      if (error != null) {
        CheckErrorAlgorithm algorithm = instantiateAlgorithm(errorNumber);
        if (algorithm != null) {
          algorithm.setConfiguration(
              wiki.getWikiConfiguration(),
              wiki.getCWConfiguration(),
              wiki.getConfiguration());
          algorithms.add(algorithm);
        }
      }
    }
    algorithmsMap.put(wiki, algorithms);
  }

  /**
   * Instantiate an algorithm.
   * 
   * @param errorNumber Algorithm number.
   * @return Algorithm or null if it doesn't exist.
   */
  private static CheckErrorAlgorithm instantiateAlgorithm(int errorNumber) {
    String className = String.format(
        "%s%03d",
        CheckErrorAlgorithm.class.getName(),
        errorNumber);
    try {
      Class<?> algorithmClass;
      try {
        algorithmClass = Class.forName(className);
      } catch (ClassNotFoundException e) {
        className = String.format(
            "%1$s.a%3$01dxx.a%4$02dx.a%5$03d.%2$s%5$03d",
            CheckErrorAlgorithm.class.getPackage().getName(),
            CheckErrorAlgorithm.class.getSimpleName(),
            errorNumber / 100,
            errorNumber / 10,
            errorNumber);
        algorithmClass = Class.forName(className);
      }
      return (CheckErrorAlgorithm) algorithmClass.getDeclaredConstructor().newInstance();
    } catch (ClassNotFoundException e) {
      // Not found: error not yet available in WikiCleaner.
    } catch (InvocationTargetException e) {
      System.err.println("InvocationTargetException for " + className);
    } catch (NoSuchMethodException e) {
      System.err.println("NoSuchMethodException for " + className);
    } catch (InstantiationException e) {
      System.err.println("InstantiationException for " + className);
    } catch (IllegalAccessException e) {
      System.err.println("IllegalAccessException for " + className);
    } catch (ClassCastException e) {
      System.err.println(
          "Class " + className +
          " doesn't implement " + CheckErrorAlgorithm.class.getName());
    }
    return null;
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
      return List.of();
    }
    List<CheckErrorAlgorithm> tmpAlgorithms = new ArrayList<>();
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
        result.add(getAlgorithm(wiki, algorithm));
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
