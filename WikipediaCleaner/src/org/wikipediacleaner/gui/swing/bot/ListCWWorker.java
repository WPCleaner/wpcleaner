/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWikiController;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumQueryResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.dump.DumpProcessor;
import org.wikipediacleaner.api.dump.PageProcessor;
import org.wikipediacleaner.api.execution.MediaWikiCallable;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.BasicWorker;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for listing Check Wiki errors from a dump.
 */
public class ListCWWorker extends BasicWorker {

  /** File containing the dump */
  private final File dumpFile;

  /** Directory (or file with place holder for error number) in which the output is written */
  private final File output;

  /** Page name (with place holder for error number) in which the output is written */
  private final String pageName;

  /** Algorithms for which to analyze pages */
  final List<CheckErrorAlgorithm> selectedAlgorithms;

  /** True if article should be checked on wiki */
  final boolean checkWiki;

  /** True to just check the pages that have been previously reported */
  final boolean onlyRecheck;

  /** List of errors found for each algorithm */
  final Map<CheckErrorAlgorithm, Map<String, Detection>> detections;

  /** Count of pages analyzed */
  int countAnalyzed;

  /** Count of pages found with errors */
  int countDetections;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param dumpFile File containing the dump to be analyzed.
   * @param output Directory (or file with place holder for error number) in which the output is written.
   * @param selectedAlgorithms List of selected algorithms.
   * @param checkWiki True if last version of articles should be checked on wiki.
   */
  public ListCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      File dumpFile, File output,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      boolean checkWiki) {
    super(wiki, window);
    this.dumpFile = dumpFile;
    this.output = output;
    this.pageName = null;
    this.selectedAlgorithms = selectedAlgorithms;
    this.detections = new HashMap<>();
    this.countAnalyzed = 0;
    this.countDetections = 0;
    this.checkWiki = checkWiki;
    this.onlyRecheck = false;
  }

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param dumpFile File containing the dump to be analyzed.
   * @param pageName Page name (with place holder for error number) in which the output is written.
   * @param selectedAlgorithms List of selected algorithms.
   * @param checkWiki True if last version of articles should be checked on wiki.
   * @param onlyRecheck True to just check the pages that have been previously reported.
   */
  public ListCWWorker(
      EnumWikipedia wiki, BasicWindow window,
      File dumpFile, String pageName,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      boolean checkWiki, boolean onlyRecheck) {
    super(wiki, window);
    this.dumpFile = dumpFile;
    this.output = null;
    this.pageName = pageName;
    this.selectedAlgorithms = selectedAlgorithms;
    this.detections = new HashMap<>();
    this.countAnalyzed = 0;
    this.countDetections = 0;
    this.checkWiki = checkWiki;
    this.onlyRecheck = onlyRecheck;
  }

  /** 
   * Compute the value to be returned by the <code>get</code> method. 
   * 
   * @return Object returned by the <code>get</code> method.
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#construct()
   */
  @Override
  public Object construct() {
    if ((dumpFile == null) || !dumpFile.canRead() || !dumpFile.isFile()) {
      return null;
    }
    if ((output == null) && (pageName == null)) {
      return null;
    }
    if (output != null) {
      if (!output.canWrite()) {
        return null;
      }
      if (!output.getName().contains("{0}") && !output.isDirectory()) {
        return null;
      }
    }
    if ((selectedAlgorithms == null) || selectedAlgorithms.isEmpty()) {
      return null;
    }
    CWPageProcessor pageProcessor = new CWPageProcessor(getWikipedia(), this);
    if (onlyRecheck) {
      try {
        List<Page> outputPages = new ArrayList<>();
        for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
          String truePageName = MessageFormat.format(pageName, algorithm.getErrorNumberString());
          Page page = DataManager.getPage(getWikipedia(), truePageName, null, null, null);
          outputPages.add(page);
        }
        API api = APIFactory.getAPI();
        api.retrieveLinks(getWikipedia(), outputPages);
        for (Page page : outputPages) {
          List<Page> links = page.getLinks();
          if (links != null) {
            for (Page link : links) {
              pageProcessor.addPage(link);
            }
          }
        }
      } catch (APIException e) {
        // Nothing to do
      }
    }
    DumpProcessor dumpProcessor = new DumpProcessor(pageProcessor);
    dumpProcessor.processDump(dumpFile);
    while (!pageProcessor.hasFinished()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        // Nothing to do
      }
    }
    for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
      Map<String, Detection> pages = detections.get(algorithm);
      if (pages == null) {
        pages = new HashMap<>();
      }
      outputResult(algorithm, pages.values());
    }

    return null;
  }

  /**
   * @param pages List of detections.
   * @return Formatted result.
   */
  private String generateResult(List<Detection> pages, Long maxSize) {
    StringBuilder buffer = new StringBuilder();
    buffer.append("<!-- Generated using " + dumpFile.getName() + " -->\n");
    ErrorLevel lastLevel = null;
    StringBuilder line = new StringBuilder();
    List<Detection> pagesToRemove = new ArrayList<>();
    for (Detection detection : pages) {
      line.setLength(0);
      if ((detection.maxLevel != null) &&
          !detection.maxLevel.equals(lastLevel)) {
        lastLevel = detection.maxLevel;
        line.append("<!-- " + lastLevel.toString() + " -->\n");
      }
      line.append("* ");
      line.append(PageElementInternalLink.createInternalLink(
          detection.pageName, null));
      line.append(": ");
      if (detection.notices != null) {
        boolean first = true;
        for (String notice : detection.notices) {
          if (!first) {
            line.append(", ");
          }
          first = false;
          line.append("<nowiki>");
          notice = notice.replaceAll("\n", "\u21b5"); // Replace \n by a visual character
          notice = notice.replaceAll("\\<", "&lt;"); // Replace "<" by its HTML element
          notice = notice.replaceAll("\u007F", "[DEL]"); // Replace control characters by visible text
          notice = notice.replaceAll("\u00A0", "[NBSP]");
          notice = notice.replaceAll("\u00AD", "[SHY]");
          notice = notice.replaceAll("\u2004", "[3EM]");
          notice = notice.replaceAll("\u2005", "[4EM]");
          notice = notice.replaceAll("\u2006", "[6EM]");
          notice = notice.replaceAll("\u2007", "[FS]");
          notice = notice.replaceAll("\u2008", "[PS]");
          notice = notice.replaceAll("\u2004", "[3EM]");
          notice = notice.replaceAll("\u200B", "[0WS]");
          notice = notice.replaceAll("\u200E", "[LRM]");
          notice = notice.replaceAll("\u2028", "[LS]");
          notice = notice.replaceAll("\u202A", "[LRE]");
          notice = notice.replaceAll("\u202C", "[POPD]");
          notice = notice.replaceAll("\uFEFF", "[BOM]");
          line.append(notice);
          line.append("</nowiki>");
        }
      }
      line.append("\n");
      if ((maxSize == null) ||
          (buffer.length() + line.length() < maxSize)) {
        buffer.append(line);
      } else {
        pagesToRemove.add(detection);
      }
    }
    pages.removeAll(pagesToRemove);
    return buffer.toString();
  }

  /**
   * Output result of the analysis.
   * 
   * @param algorithm Algorithm.
   * @param pages List of pages with detections.
   */
  private void outputResult(CheckErrorAlgorithm algorithm, Collection<Detection> pages) {
    if ((algorithm == null) || (pages == null)) {
      return;
    }

    // Prepare result
    List<Detection> tmpPages = new ArrayList<>(pages);
    Collections.sort(tmpPages);
    int nbPages = tmpPages.size();
    String result = generateResult(tmpPages, null);

    // Output to file
    if (output != null) {
      File outputFile = null;
      if (!output.getName().contains("{0}")) {
        outputFile = new File(
            output,
            "CW_" + getWikipedia().getSettings().getCodeCheckWiki() + "_" + algorithm.getErrorNumberString() + ".txt");
      } else {
        outputFile = new File(MessageFormat.format(output.getAbsolutePath(), algorithm.getErrorNumberString()));
      }
      BufferedWriter writer = null;
      try {
        writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFile, false), "UTF8"));
        writer.write(result);
      } catch (IOException e) {
        // Nothing to do
      } finally {
        if (writer != null) {
          try {
            writer.close();
          } catch (IOException e) {
            // Nothing to do
          }
        }
      }
    }

    // Output to a page
    if (pageName != null) {
      boolean finished = false;
      while (!finished) {
        try {
          finished = true;
          String truePageName = MessageFormat.format(pageName, algorithm.getErrorNumberString());
          Page page = DataManager.getPage(getWikipedia(), truePageName, null, null, null);
          API api = APIFactory.getAPI();
          api.retrieveContents(getWikipedia(), Collections.singletonList(page), false, false);
          String contents = page.getContents();
          if (contents != null) {
            int begin = -1;
            int end = -1;
            for (PageElementComment comment : page.getAnalysis(contents, true).getComments()) {
              String value = comment.getComment().trim();
              if ("BOT BEGIN".equals(value)) {
                if (begin < 0) {
                  begin = comment.getEndIndex();
                }
              } else if ("BOT END".equals(value)) {
                end = comment.getBeginIndex();
              }
            }
            if ((begin >= 0) && (end > begin)) {
              String text = null;
              finished = false;
              while (!finished) {
                StringBuilder newText = new StringBuilder();
                newText.append(contents.substring(0, begin));
                newText.append("\n");
                newText.append(result);
                newText.append(contents.substring(end));
                text = newText.toString();
                if (!getWikipedia().getWikiConfiguration().isArticleTooLong(text) ||
                    tmpPages.isEmpty()) {
                  finished = true; 
                } else {
                  tmpPages.remove(tmpPages.size() - 1);
                  result = generateResult(
                      tmpPages,
                      getWikipedia().getWikiConfiguration().getMaxArticleSize());
                }
              }
              try {
                api.updatePage(
                    getWikipedia(), page, text,
                    "Dump analysis for error n°" + algorithm.getErrorNumberString() + " (" + nbPages + " pages)",
                    true, false);
              } catch (APIException e) {
                if (EnumQueryResult.CONTENT_TOO_BIG.equals(e.getQueryResult())) {
                  for (int i = 0; i < 100; i++) {
                    if (!tmpPages.isEmpty()) {
                      finished = false;
                      tmpPages.remove(tmpPages.size() - 1);
                    }
                  }
                  result = generateResult(
                      tmpPages,
                      getWikipedia().getWikiConfiguration().getMaxArticleSize());
                } else {
                  throw e;
                }
              }
            }
          }
        } catch (APIException e) {
          // Nothing
        }
      }
    }
  }

  /**
   * Called on the event dispatching thread (not on the worker thread)
   * after the <code>construct</code> method has returned.
   * 
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#finished()
   */
  /**
   * 
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorker#finished()
   */
  @Override
  public void finished() {
    super.finished();
    if (getWindow() != null) {
      StringBuilder message = new StringBuilder();
      message.append(GT.__(
          "{0} page has been analyzed",
          "{0} pages have been analyzed",
          countAnalyzed, Integer.toString(countAnalyzed)));
      for (Entry<CheckErrorAlgorithm, Map<String, Detection>> error : detections.entrySet()) {
        if ((error != null) && (error.getKey() != null) && (error.getValue() != null)) {
          CheckErrorAlgorithm algorithm = error.getKey();
          Map<String, Detection> pages = error.getValue();
          message.append("\n");
          message.append(GT.__(
              "{0} page has been detected for algorithm {1}",
              "{0} pages have been detected for algorithm {1}",
              pages.size(), new Object[] {
                pages.size(),
                algorithm.getErrorNumberString() + " - " + algorithm.getShortDescription()}));
        }
      }
      Utilities.displayInformationMessage(
          getWindow().getParentComponent(), message.toString());
    }
  }

  /**
   * Controller for background tasks.
   */
  private class CWController extends MediaWikiController {

    /**
     * @param listener Listener to MediaWiki events.
     */
    public CWController(MediaWikiListener listener) {
      super(listener);
    }

    /**
     * @param task Task to be performed in background.
     * @see org.wikipediacleaner.api.MediaWikiController#addTask(java.util.concurrent.Callable)
     */
    @Override
    public void addTask(Callable<?> task) {
      hasFinished(); // To clean up done tasks
      super.addTask(task);
    }

    /**
     * @return True if all tasks are completed.
     */
    public boolean hasFinished() {
      if (hasRemainingTask()) {
        Future<?> result = getNextDoneResult();
        if (result == null) {
          return false;
        }
      }
      return !hasRemainingTask();
    }
  }

  /**
   * Background task.
   */
  private class CWPageCallable extends MediaWikiCallable<Page> {

    /** Page to analyze */
    private final Page page;

    /**
     * @param wikipedia Wikipedia.
     * @param listener Listener of MediaWiki events.
     * @param api MediaWiki API.
     * @param page Page.
     * @param checkWiki True if last version should be checked on wiki.
     */
    public CWPageCallable(
        EnumWikipedia wiki, MediaWikiListener listener, API api,
        Page page) {
      super(wiki, listener, api);
      this.page = page;
    }

    /* (non-Javadoc)
     * @see java.util.concurrent.Callable#call()
     */
    @Override
    public Page call() throws APIException {
      EnumWikipedia wiki = getWikipedia();
      PageAnalysis analysis = page.getAnalysis(page.getContents(), false);
      Page currentPage = null;
      PageAnalysis currentAnalysis = null; 
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        List<CheckErrorResult> errors = new ArrayList<>();
        if (!algorithm.isInWhiteList(page.getTitle()) &&
            algorithm.analyze(analysis, errors, false)) {
          boolean detectionConfirmed = false;

          // Confirm detection
          if (checkWiki) {
            try {
              if (currentPage == null) {
                currentPage = DataManager.getPage(wiki, page.getTitle(), null, null, null);
              }
              if (currentAnalysis == null) {
                api.retrieveContents(wiki, Collections.singleton(currentPage), false, false);
                currentAnalysis = currentPage.getAnalysis(currentPage.getContents(), false);
              }
              errors.clear();
              if (algorithm.analyze(currentAnalysis, errors, false)) {
                detectionConfirmed = true;
              }
            } catch (APIException e) {
              // Nothing to do
            }
          } else {
            detectionConfirmed = true;
            currentPage = page;
          }

          // Memorize detection
          if (detectionConfirmed) {
            System.out.println(
                "Detection confirmed for " + page.getTitle() +
                ": " + algorithm.getErrorNumberString() +
                " - " + algorithm.getShortDescription());
            Map<String, Detection> pages = detections.get(algorithm);
            if (pages == null) {
              pages = new HashMap<>();
              detections.put(algorithm, pages);
            }
            pages.put(currentPage.getTitle(), new Detection(currentPage, errors));
            countDetections++;
          }
        }
      }
      countAnalyzed++;
      if (countAnalyzed % 100000 == 0) {
        System.out.println(
            "Pages processed: " + countAnalyzed +
            " / errors detected: " + countDetections);
      }
      if (countAnalyzed % 1000 == 0) {
        setText(GT._("{0} pages processed", Integer.toString(countAnalyzed)));
      }
      return page;
    }
  }

  /**
   * Process pages in the dump.
   */
  private class CWPageProcessor implements PageProcessor {

    /** Wiki */
    private final EnumWikipedia wiki;

    /** Listener */
    private final MediaWikiListener listener;

    /** Controller for background tasks */
    private final CWController controller;

    /** API */
    private final API api;

    /** Restrict the processing to this list of pages */
    private List<String> pagesList;

    /**
     * @param wiki Wiki.
     * @param listener Listener.
     */
    public CWPageProcessor(EnumWikipedia wiki, MediaWikiListener listener) {
      this.wiki = wiki;
      this.listener = listener;
      this.controller = new CWController(null);
      this.api = APIFactory.getAPI();
    }

    /**
     * @return Wiki.
     * @see org.wikipediacleaner.api.dump.PageProcessor#getWiki()
     */
    @Override
    public EnumWikipedia getWiki() {
      return wiki;
    }

    /**
     * Add a page to the list of pages to check.
     * 
     * @param page Page to be checked.
     */
    public void addPage(Page page) {
      if (page == null) {
        return;
      }
      String title = page.getTitle();
      if (pagesList == null) {
        pagesList = new ArrayList<>();
      }
      if (!pagesList.contains(title)) {
        pagesList.add(title);
      }
    }

    /**
     * @param page Page.
     * @see org.wikipediacleaner.api.dump.PageProcessor#processPage(org.wikipediacleaner.api.data.Page)
     */
    @Override
    public void processPage(Page page) {
      if ((page != null) && page.isInMainNamespace()) {
        if ((pagesList == null) || pagesList.contains(page.getTitle())) {
          controller.addTask(new CWPageCallable(wiki, listener, api, page));
        }
      }
    }

    /**
     * @return True if all tasks are completed.
     */
    public boolean hasFinished() {
      return controller.hasFinished();
    }
  }

  /**
   * Bean for holding detection results.
   */
  static class Detection implements Comparable<Detection> {

    /** Page name */
    public final String pageName;

    /** List of notices */
    public final List<String> notices;

    /** Maximum level for the errors */
    public final ErrorLevel maxLevel;

    /**
     * @param page Page.
     * @param errors List of errors.
     */
    public Detection(Page page, List<CheckErrorResult> errors) {
      this.pageName = page.getTitle();
      this.notices = new ArrayList<>();
      ErrorLevel tmpLevel = ErrorLevel.CORRECT;
      if (errors != null) {
        for (CheckErrorResult error : errors) {
          String contents = page.getContents();
          if (contents != null) {
            notices.add(new String(contents.substring(
                error.getStartPosition(), error.getEndPosition())));
          }
          ErrorLevel currentLevel = error.getErrorLevel();
          if (currentLevel.ordinal() < tmpLevel.ordinal()) {
            tmpLevel = currentLevel;
          }
        }
      }
      this.maxLevel = tmpLevel;
    }

    /**
     * @param o
     * @return
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(Detection o) {
      if (o == null) {
        return -1;
      }

      // Compare error level
      if (!maxLevel.equals(o.maxLevel)) {
        if (maxLevel.ordinal() < o.maxLevel.ordinal()) {
          return -1;
        }
        return 1;
      }

      // Compare pages
      if (pageName == null) {
        if (o.pageName == null) {
          return 0;
        }
        return 1;
      }
      if (o.pageName == null) {
        return -1;
      }
      return pageName.compareTo(o.pageName);
    }
  }
}
