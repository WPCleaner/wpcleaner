/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.bot;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.MediaWikiController;
import org.wikipediacleaner.api.MediaWikiListener;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.analysis.AnalysisPerformance;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.dump.DumpProcessor;
import org.wikipediacleaner.api.dump.PageProcessor;
import org.wikipediacleaner.api.execution.MediaWikiCallable;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;


/**
 * SwingWorker for fixing errors from a dump.
 */
public class FixDumpWorker extends AutomaticFixWorker {

  /** Logger */
  private final static Logger log = LoggerFactory.getLogger(FixDumpWorker.class);

  /** Logger CW */
  final static Logger logCW = LoggerFactory.getLogger("DumpAnalysis");

  /** File containing the dump */
  private final File dumpFile;

  /** Time spent in analysis. */
  AnalysisPerformance analysisTime;

  /** Count of pages analyzed */
  int countAnalyzed;

  /**
   * @param wiki Wiki.
   * @param window Window.
   * @param dumpFile File containing the dump to be analyzed.
   * @param selectedAlgorithms List of selected algorithms.
   * @param allAlgorithms List of possible algorithms.
   * @param selectedNamespaces List of selected namespaces.
   */
  public FixDumpWorker(
      EnumWikipedia wiki, BasicWindow window,
      File dumpFile,
      List<CheckErrorAlgorithm> selectedAlgorithms,
      List<CheckErrorAlgorithm> allAlgorithms,
      Collection<Integer> selectedNamespaces) {
    super(
        wiki, window,
        selectedAlgorithms, allAlgorithms,
        selectedNamespaces,
        null, true, false, false);
    this.dumpFile = dumpFile;
    this.analysisTime = new AnalysisPerformance();
    this.countAnalyzed = 0;
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
    if ((selectedAlgorithms == null) || selectedAlgorithms.isEmpty()) {
      return null;
    }
    CWPageProcessor pageProcessor = new CWPageProcessor(getWikipedia(), this, selectedNamespaces);
    DumpProcessor dumpProcessor = new DumpProcessor(pageProcessor);
    dumpProcessor.processDump(dumpFile);
    while (!pageProcessor.hasFinished()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        // Nothing to do
      }
    }

    return null;
  }

  /**
   * Report progress.
   */
  void reportProgress() {
    StringBuilder buffer = new StringBuilder();
    buffer.append("\n");
    buffer.append("Pages processed: " + countAnalyzed);
    log.info(buffer.toString());
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

    // Build final message
    StringBuilder message = new StringBuilder();
    message.append(GT.__(
        "{0} page has been analyzed",
        "{0} pages have been analyzed",
        countAnalyzed, Integer.toString(countAnalyzed)));

    // Log final message
    logCW.info(message.toString());

    // Display final message
    if (getWindow() != null) {
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
      // hasFinished(); // To clean up done tasks
      while (getRemainingTasksCount() > 10000) {
        try {
          // logCW.info("Too many tasks remaining, waiting a bit");
          TimeUnit.MILLISECONDS.sleep(1000);
        } catch (InterruptedException e) {
          // Do nothing
        }
        cleanUpDone();
      }
      super.addTask(task);
      cleanUpDone();
    }

    /**
     * Clean up tasks that are done and finished.
     */
    private void cleanUpDone() {
      while (getFirstResultIfDone() != null) {
        // Do nothing, done result is simply removed from the list of tasks to clean up done tasks
      }
    }

    /**
     * @return True if all tasks are completed.
     */
    public boolean hasFinished() {
      while (hasRemainingTask()) {
        Future<?> result = getFirstResultIfDone();
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
      analysis.performFullPageAnalysis(analysisTime);

      // Check if an automatic fix can be applied
      boolean automaticFix = false;
      String initialContents = page.getContents();
      for (CheckErrorAlgorithm algorithm : selectedAlgorithms) {
        if (!automaticFix && !algorithm.isInWhiteList(page.getTitle())) {
          if (algorithm.analyze(analysis, null, true)) {
            String newContents = algorithm.automaticFix(analysis);
            automaticFix = !newContents.equals(initialContents);
          }
        }
      }

      // If an automatic fix can be applied, try on the online page
      if (automaticFix) {
        Page currentPage = DataManager.getPage(wiki,  page.getTitle(), null, null, null);
        analyzePage(currentPage, selectedAlgorithms, null);
      }

      countAnalyzed++;
      if (countAnalyzed % 100000 == 0) {
        reportProgress();
      }
      if (countAnalyzed % 1000 == 0) {
        setText(GT._T("{0} pages processed", Integer.toString(countAnalyzed)));
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

    /** Namespaces to be analyzed */
    private final Set<Integer> namespaces;

    /** Controller for background tasks */
    private final CWController controller;

    /** API */
    private final API api;

    /**
     * @param wiki Wiki.
     * @param listener Listener.
     * @param namespaces Namespaces to be analyzed.
     */
    public CWPageProcessor(
        EnumWikipedia wiki,
        MediaWikiListener listener,
        Set<Integer> namespaces) {
      this.wiki = wiki;
      this.listener = listener;
      this.namespaces = new HashSet<>();
      if (namespaces != null) {
        this.namespaces.addAll(namespaces);
      }
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
     * Tells if the processor should work on a namespace.
     * 
     * @param namespace Namespace.
     * @return True if the processor should work on the namespace.
     * @see org.wikipediacleaner.api.dump.PageProcessor#isForNamespace(Integer)
     */
    @Override
    public boolean isForNamespace(Integer namespace) {
      return namespaces.contains(namespace);
    }

    /**
     * @param page Page.
     * @see org.wikipediacleaner.api.dump.PageProcessor#processPage(org.wikipediacleaner.api.data.Page)
     */
    @Override
    public void processPage(Page page) {
      if (page == null) {
        return;
      }
      controller.addTask(new CWPageCallable(wiki, listener, api, page));
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

    /** Namespace */
    public final Integer namespace;

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
      this.namespace = page.getNamespace();
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

      // Compare namespaces
      if (namespace == null) {
        if (o.namespace != null) {
          return 1;
        }
      }
      if (o.namespace == null) {
        return -1;
      }
      if (!namespace.equals(o.namespace)) {
        return namespace.compareTo(o.namespace);
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
