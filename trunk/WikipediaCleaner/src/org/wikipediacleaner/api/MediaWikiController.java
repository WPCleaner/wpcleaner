/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

package org.wikipediacleaner.api;

import java.util.LinkedList;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.wikipediacleaner.api.base.APIException;
import org.wikipediacleaner.utils.Configuration;


/**
 * Centralisation of access to MediaWiki.
 */
public abstract class MediaWikiController implements MediaWikiListener {

  static private ExecutorService staticExecutor;

  private final MediaWikiListener listener;
  private final ExecutorService executor;
  private final LinkedList<Future<?>> results;

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
          Configuration.INTEGER_INTERROG_THREAD,
          Configuration.DEFAULT_INTERROG_THREAD);
      staticExecutor = Executors.newFixedThreadPool(nThreads);
    }
    return staticExecutor;
  }

  /**
   * @param task
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
    Future<?> result = executor.submit(task);
    synchronized (results) {
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
      synchronized (results) {
        for (Future<?> result : results) {
          if (result.isDone()) {
            results.remove(result);
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
   * @param text Text to display.
   */
  public void setText(String text) {
    if (listener != null) {
      listener.setText(text);
    }
  }

  /**
   * @return Flag indicating if the processing should stop.
   */
  public boolean shouldStop() {
    if (listener != null) {
      return listener.shouldStop();
    }
    return false;
  }
}
