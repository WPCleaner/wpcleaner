/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    if ((errorNumber < 1) || (errorNumber > CWConfiguration.MAX_ERROR_NUMBER)) {
      return null;
    }
    return algorithms.get(errorNumber - 1);
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

  /**
   * Retrieve error priority from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Priority.
   */
  public static int getPriority(
      EnumWikipedia wikipedia, int errorNumber) {
    CWConfigurationError error = wikipedia.getCWConfiguration().getErrorConfiguration(errorNumber);
    if (error == null) {
      return CWConfigurationError.PRIORITY_UNKOWN;
    }
    return error.getPriority();
  }

  /**
   * Retrieve error short description from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Short description.
   */
  public static String getShortDescription(
      EnumWikipedia wikipedia, int errorNumber) {
    CWConfigurationError error = wikipedia.getCWConfiguration().getErrorConfiguration(errorNumber);
    if (error == null) {
      return null;
    }
    return error.getShortDescription();
  }

  /**
   * Retrieve error long description from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Long description.
   */
  public static String getLongDescription(
      EnumWikipedia wikipedia, int errorNumber) {
    CWConfigurationError error = wikipedia.getCWConfiguration().getErrorConfiguration(errorNumber);
    if (error == null) {
      return null;
    }
    return error.getLongDescription();
  }

  /**
   * Retrieve link to error description from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return Link to error description.
   */
  public static String getLink(
      EnumWikipedia wikipedia, int errorNumber) {
    CWConfigurationError error = wikipedia.getCWConfiguration().getErrorConfiguration(errorNumber);
    if (error == null) {
      return null;
    }
    return error.getLink();
  }

  /**
   * Retrieve white list from configuration.
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @return White list
   */
  public static String[] getWhiteList(
      EnumWikipedia wikipedia, int errorNumber) {
    CWConfigurationError error = wikipedia.getCWConfiguration().getErrorConfiguration(errorNumber);
    if (error == null) {
      return null;
    }
    return error.getWhiteList();
  }
}
