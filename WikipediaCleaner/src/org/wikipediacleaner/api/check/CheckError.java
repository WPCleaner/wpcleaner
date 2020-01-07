/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithms;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Performance;


/**
 * Managing errors defined in the check wikipedia project.
 */
public class CheckError {

  /** Flag to trace time spent in each analysis */
  private static boolean traceTime = false;

  /**
   * @param trace True to force tracing time spent in analysis.
   */
  public static void setTraceTime(boolean trace) {
    traceTime = trace;
  }

  /**
   * Analyze a page to find error types.
   * 
   * @param algorithms Possible algorithms.
   * @param pageAnalysis Page analysis.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Errors found in the page.
   */
  public static List<CheckErrorPage> analyzeErrors(
      Collection<CheckErrorAlgorithm> algorithms,
      PageAnalysis pageAnalysis,
      boolean onlyAutomatic) {
    Performance perf = null;
    if (traceTime) {
      perf = Performance.getInstance("CheckError.analyzeErrors");
      perf.printStart();
    }
    List<CheckErrorPage> errorsFound = new ArrayList<CheckErrorPage>();
    if ((algorithms != null) &&
        (pageAnalysis != null) &&
        (pageAnalysis.getContents() != null)) {
      for (CheckErrorAlgorithm algorithm : algorithms) {
        if ((algorithm != null) &&
            (algorithm.isAvailable()) &&
            (CWConfigurationError.isPriorityActive(algorithm.getPriority()))) {
          List<CheckErrorResult> results = new ArrayList<CheckErrorResult>();
          boolean errorFound = false;
          int errorNumber = algorithm.getErrorNumber();
          PageAnalysis.Result result = pageAnalysis.getCheckWikiErrors(errorNumber);
          if (result != null) {
            errorFound = result.getErrors(results);
          } else {
            errorFound = algorithm.analyze(pageAnalysis, results, onlyAutomatic);
            pageAnalysis.setCheckWikiErrors(errorNumber, errorFound, results);
          }
          if (errorFound) {
            CheckErrorPage errorPage = new CheckErrorPage(pageAnalysis.getPage(), algorithm);
            errorPage.setResults(true, results);
            errorsFound.add(errorPage);
          }
          if (perf != null) {
            String message =
                "Error n°" + algorithm.getErrorNumber() +
                ", " + errorFound +
                ", " + results.size() + " occurrences";
            perf.printStep(message);
          }
        }
      }
    }
    if (perf != null) {
      perf.printEnd();
      perf.release();
    }
    return errorsFound;
  }

  /**
   * Analyze a page to find errors of a given type.
   * 
   * @param algorithm Algorithm.
   * @param pageAnalysis Page analysis.
   * @return Error page.
   */
  public static CheckErrorPage analyzeError(
      CheckErrorAlgorithm algorithm, PageAnalysis pageAnalysis) {
    if ((algorithm == null) || (pageAnalysis == null)) {
      return null;
    }
    Performance perf = null;
    if (traceTime) {
      perf = Performance.getInstance("CheckError.analyzeError");
    }
    CheckErrorPage errorPage = new CheckErrorPage(pageAnalysis.getPage(), algorithm);
    boolean errorFound = false;
    List<CheckErrorResult> errorsFound = new ArrayList<CheckErrorResult>();
    int errorNumber = algorithm.getErrorNumber();
    PageAnalysis.Result result = pageAnalysis.getCheckWikiErrors(errorNumber);
    if (result != null) {
      errorFound = result.getErrors(errorsFound);
    } else {
      errorFound = algorithm.analyze(pageAnalysis, errorsFound, false);
      pageAnalysis.setCheckWikiErrors(errorNumber, errorFound, errorsFound);
    }
    errorPage.setResults(errorFound, errorsFound);
    if (perf != null) {
      perf.printStep("Error n°" + algorithm.getErrorNumber());
      perf.release();
    }
    return errorPage;
  }

  /**
   * @param initialErrors List of initial errors.
   * @param contents Current contents.
   * @param shouldCheckSpelling True if spelling should be checked.
   * @return Information about errors fixed.
   */
  public static List<Progress> computeErrorsFixed(
      List<CheckErrorPage> initialErrors,
      String contents, boolean shouldCheckSpelling) {
    final List<Progress> errorsFixed = new ArrayList<>();
    PageAnalysis analysis = null;
    if (initialErrors != null) {
      for (CheckErrorPage initialError : initialErrors) {
        if (analysis == null) {
          analysis = initialError.getPage().getAnalysis(contents, true);
          analysis.shouldCheckSpelling(shouldCheckSpelling);
        }
        CheckErrorPage errorPage = analyzeError(
            initialError.getAlgorithm(), analysis);
        if ((errorPage.getErrorFound() == false) ||
            (errorPage.getActiveResultsCount() < initialError.getActiveResultsCount())) {
          errorsFixed.add(new Progress(initialError.getAlgorithm(), errorPage.getErrorFound() == false));
        }
      }
    }
    return errorsFixed;
  }

  /**
   * @param errors Errors list.
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @param stream Stream containing list of pages for the error number.
   */
  public static void addCheckErrorClassic(
      List<CheckError> errors,
      EnumWikipedia wikipedia, int errorNumber, InputStream stream) {

    // Analyze properties to find informations about error number
    if (!CheckErrorAlgorithms.isAlgorithmActive(wikipedia, errorNumber)) {
      return;
    }

    // Create error
    CheckError error = new CheckError(wikipedia, errorNumber);
    if (stream != null) {
      BufferedReader reader = null;
      try {
        reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
        String line = null;
        while (((line = reader.readLine()) != null) && !line.endsWith("<pre>")) {
          // Waiting for <pre>
        }
        while (((line = reader.readLine()) != null) && !line.startsWith("</pre>")) {
          line = line.replaceAll(Pattern.quote("&#039;"), "'");
          line = line.replaceAll(Pattern.quote("&quot;"), "\"");
          line = line.replaceAll(Pattern.quote("&amp;"), "&");
          error.addPage(line, null);
        }
      } catch (UnsupportedEncodingException e) {
        //
      } catch (IOException e) {
        //
      } finally {
        if (reader != null) {
          try {
            reader.close();
          } catch (IOException e) {
            //
          }
        }
      }
    }

    // Add / Replace error
    for (int i = errors.size(); i > 0; i--) {
      if (errors.get(i - 1).getErrorNumber() == errorNumber) {
        errors.remove(i - 1);
      }
    }
    errors.add(error);
  }

  /**
   * @param errors Errors list.
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @param stream Stream containing list of pages for the error number.
   */
  public static void addCheckErrorBots(
      List<CheckError> errors,
      EnumWikipedia wikipedia, int errorNumber, InputStream stream) {

    // Analyze properties to find informations about error number
    if (!CheckErrorAlgorithms.isAlgorithmActive(wikipedia, errorNumber)) {
      return;
    }

    // Create error
    CheckError error = new CheckError(wikipedia, errorNumber);
    if (stream != null) {
      BufferedReader reader = null;
      try {
        // Read all lines
        reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
        ArrayList<String> lines = new ArrayList<String>();
        String line = null;
        while ((line = reader.readLine()) != null) {
          lines.add(line);
        }

        if ((lines.size() == 1) && (lines.get(0).indexOf("\\n") > 0)) {
          String[] splittedLines = lines.get(0).split("\\\\n");
          lines.clear();
          for (String splittedLine : splittedLines) {
            lines.add(splittedLine);
          }
        }
        for (String tmpLine : lines) {
          String[] elements = tmpLine.split("\\|");
          String pageName = null;
          Integer pageId = null;
          for (String element : elements) {
            int equalIndex = element.indexOf("=");
            if (equalIndex > 0) {
              String attribute = element.substring(0, equalIndex);
              if ("title".equals(attribute)) {
                pageName = element.substring(equalIndex + 1);
              } else if ("pageid".equals(attribute)) {
                try {
                  pageId = Integer.valueOf(element.substring(equalIndex + 1));
                } catch (NumberFormatException e) {
                  //
                }
              }
            }
          }
          // System.err.println("Line: " + tmpLine);
          if ((pageName != null) && (pageName.trim().length() > 0)) {
            pageName = pageName.replaceAll(Pattern.quote("&#039;"), "'");
            pageName = pageName.replaceAll(Pattern.quote("&quot;"), "\"");
            pageName = pageName.replaceAll(Pattern.quote("&amp;"), "&");
            error.addPage(pageName, pageId);
          }
        }
      } catch (UnsupportedEncodingException e) {
        //
      } catch (IOException e) {
        //
      } finally {
        if (reader != null) {
          try {
            reader.close();
          } catch (IOException e) {
            //
          }
        }
      }
    }

    // Add / Replace error
    for (int i = errors.size(); i > 0; i--) {
      if (errors.get(i - 1).getErrorNumber() == errorNumber) {
        errors.remove(i - 1);
      }
    }
    errors.add(error);
  }

  /**
   * @param errors Errors list.
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number.
   * @param pages List of pages in error.
   */
  public static void addCheckErrorPages(
      List<CheckError> errors,
      EnumWikipedia wikipedia, int errorNumber,
      List<Page> pages) {

    // Analyze properties to find informations about error number
    if (!CheckErrorAlgorithms.isAlgorithmActive(wikipedia, errorNumber)) {
      return;
    }

    // Check that the list of pages in error is not empty
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }

    // Create error
    CheckError error = new CheckError(wikipedia, errorNumber);
    for (Page page : pages) {
      error.addPage(page.getTitle(), page.getPageId());
    }

    // Add / Replace error
    for (int i = errors.size(); i > 0; i--) {
      if (errors.get(i - 1).getErrorNumber() == errorNumber) {
        errors.remove(i - 1);
      }
    }
    errors.add(error);
  }

  private final EnumWikipedia wikipedia;
  private final int errorNumber;
  private final CheckErrorAlgorithm algorithm;
  private final List<Page> errors;

  /**
   * Constructor
   * 
   * @param wikipedia Wikipedia.
   * @param errorNumber Error number as defined in the check wikipedia project.
   */
  private CheckError(
      EnumWikipedia wikipedia, int errorNumber) {
    this.wikipedia = wikipedia;
    this.algorithm = CheckErrorAlgorithms.getAlgorithm(wikipedia, errorNumber);
    this.errorNumber = errorNumber;
    this.errors = new ArrayList<Page>();
  }
  
  /**
   * @return Error number as defined in the check wikipedia project.
   */
  public int getErrorNumber() {
    return errorNumber;
  }

  /**
   * @return Algorithm.
   */
  public CheckErrorAlgorithm getAlgorithm() {
    return algorithm;
  }

  /**
   * @return Number of error pages.
   */
  public int getPageCount() {
    return errors.size();
  }

  /**
   * @param index Page index.
   * @return Error page.
   */
  public Page getPage(int index) {
    if ((index < 0) || (index >= errors.size())) {
      return null;
    }
    return errors.get(index);
  }

  /**
   * Add a page to the list of errors.
   * 
   * @param page Page.
   * @param pageId Page id.
   */
  private void addPage(String page, Integer pageId) {
    Page tmpPage = DataManager.getPage(wikipedia, page, pageId, null, null);
    if (!errors.contains(tmpPage)) {
      errors.add(tmpPage);
    }
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    String count = Integer.toString(errors.size());
    int priority = CWConfigurationError.PRIORITY_UNKOWN;
    String description = GT._T("No available description");
    if (algorithm != null) {
      priority = algorithm.getPriority();
      description = algorithm.getShortDescriptionReplaced();
    }
    return GT._T("Error n°{0} ({1} - {2}) - {3}", new Object[] {
        Integer.valueOf(errorNumber),
        count,
        CWConfigurationError.getPriorityString(priority),
        description });
  }

  /**
   * Remove a page from the list of errors.
   * 
   * @param page Page.
   * @return True if a page has been removed from the list.
   */
  public boolean remove(Page page) {
    if (page == null) {
      return false;
    }
    boolean removed = false;
    synchronized (errors) {
      for (int i = errors.size(); i > 0; i--) {
        if (Page.areSameTitle(page.getTitle(), errors.get(i - 1).getTitle())) {
          errors.remove(i - 1);
          removed = true;
        }
      }
    }
    return removed;
  }

  /**
   * Bean for holding progress report on fixing errors.
   */
  public static class Progress {

    /** Algorithm */
    final public CheckErrorAlgorithm algorithm;

    /** True if error has been completely fixed */
    final public boolean full;

    /**
     * @param algorithm Algorithm.
     * @param full True if error has been completely fixed.
     */
    public Progress(CheckErrorAlgorithm algorithm, boolean full) {
      this.algorithm = algorithm;
      this.full = full;
    }
  }
}
