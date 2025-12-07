import { useEffect, useState } from 'react';
import { AggregatedStatus } from './types';
import Dashboard from './components/Dashboard';
import Header from './components/Header';

function App() {
  const [status, setStatus] = useState<AggregatedStatus | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchStatus = async () => {
    try {
      const response = await fetch('/api/status');
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      const data = await response.json();
      setStatus(data);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to fetch status');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchStatus();
    const interval = setInterval(fetchStatus, 30000); // Refresh every 30 seconds
    return () => clearInterval(interval);
  }, []);

  return (
    <div className="min-h-screen bg-gray-900">
      <Header all_ok={status?.all_ok ?? true} last_update={status?.last_update} />

      <main className="container mx-auto px-4 py-8">
        {loading && (
          <div className="flex justify-center items-center h-64">
            <div className="text-gray-400 text-xl">Loading...</div>
          </div>
        )}

        {error && (
          <div className="bg-red-900/20 border border-red-500 text-red-400 px-6 py-4 rounded-lg">
            <h3 className="font-bold mb-2">Error</h3>
            <p>{error}</p>
          </div>
        )}

        {!loading && !error && status && (
          <Dashboard status={status} onRefresh={fetchStatus} />
        )}
      </main>
    </div>
  );
}

export default App;
