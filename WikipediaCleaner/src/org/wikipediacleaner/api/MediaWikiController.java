/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.NamedThreadFactory;


/**
 * Centralization of access to MediaWiki.
 */
public abstract class MediaWikiController implements MediaWikiListener {

  static private ExecutorService staticExecutor;

  private final MediaWikiListener listener;
  private final ExecutorService executor;
  private final List<Future<?>> results;

  /**
   * Create a MediaWikiController.
   * 
   * @param listener Listener to MediaWiki events.
   */
  protected MediaWikiController(MediaWikiListener listener) {
    this.listener = listener;
    this.executor = getStaticExecutor();
    results = new LinkedList<Future<?>>();
  }

  /**
   * @return The executor.
   */
  static private synchronized ExecutorService getStaticExecutor() {
    if (staticExecutor == null) {
      Configuration config = Configuration.getConfiguration();
      int nThreads = config.getInt(
          null,
          ConfigurationValueInteger.INTERROG_THREAD);
      staticExecutor = new ThreadPoolExecutor(
          nThreads, nThreads,
          0L, TimeUnit.MILLISECONDS,
          new LinkedBlockingQueue<Runnable>(Integer.MAX_VALUE),
          new NamedThreadFactory(Executors.defaultThreadFactory(), "MW-{0}"));
    }
    return staticExecutor;
  }

  /**
   * @param task Task.
   * @return Future result
   */
  static public Future<?> addSimpleTask(Callable<?> task) {
    return getStaticExecutor().submit(task);
  }

  /**
   * Add a task in the queue of tasks to run.
   * 
   * @param task The task itself.
   */
  protected void addTask(Callable<?> task) {
    synchronized (results) {
      Future<?> result = executor.submit(task);
      if (!shouldStop()) {
        results.add(result);
      }
    }
  }

  /**
   * @return Flag indicating if there are remaining tasks to analyze.
   */
  protected boolean hasRemainingTask() {
    synchronized (results) {
      return !results.isEmpty();
    }
  }

  /**
   * @return Number of remaining tasks.
   */
  protected int getRemainingTasksCount() {
    synchronized (results) {
      return results.size();
    }
  }

  /**
   * Stop remaining tasks.
   */
  protected void stopRemainingTasks() {
    synchronized (results) {
      for (Future<?> result : results) {
        result.cancel(false);
      }
    }
  }

  /**
   * @return The result of one of the completed remaining tasks.
   * @throws APIException Exception.
   */
  protected Object getNextResult() throws APIException {
    while (hasRemainingTask()) {
      Future<?> result = getNextDoneResult();
      if (result != null) {
        try {
          return result.get(10, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
          // Nothing to do
        } catch (ExecutionException e) {
          Throwable cause = e.getCause();
          if (cause instanceof APIException) {
            throw (APIException) cause;
          }
          return cause;
        } catch (TimeoutException e) {
          // Shouldn't arrive
        } catch (CancellationException e) {
          //
        }
      }
      try {
        Thread.sleep(100);
      } catch (InterruptedException e) {
        // Nothing to do
      }
    }
    return null;
  }

  /**
   * @return The result of the first remaining tasks if it is completed.
   */
  protected Future<?> getFirstResultIfDone() {
    if (hasRemainingTask()) {
      synchronized (results) {
        if (!results.isEmpty()) {
          Future<?> result = results.get(0);
          if (result.isDone()) {
            results.remove(result);
            return result;
          }
        }
      }
    }
    return null;
  }

  /**
   * @return The result of one of the completed remaining tasks.
   */
  protected Future<?> getNextDoneResult() {
    if (hasRemainingTask()) {
      synchronized (results) {
        for (Future<?> result : results) {
          if (result.isDone()) {
            results.remove(result);
            return result;
          }
        }
      }
    }
    return null;
  }

  /**
   * @param text Text to display.
   */
  @Override
  public void setText(String text) {
    if (listener != null) {
      listener.setText(text);
    }
  }

  /**
   * @return Flag indicating if the processing should stop.
   */
  @Override
  public boolean shouldStop() {
    if (listener != null) {
      return listener.shouldStop();
    }
    return false;
  }
}
