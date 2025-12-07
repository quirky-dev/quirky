import { AggregatedStatus } from '../types';
import SatelliteCard from './SatelliteCard';

interface DashboardProps {
  status: AggregatedStatus;
  onRefresh: () => void;
}

export default function Dashboard({ status, onRefresh }: DashboardProps) {
  const satellites = Object.entries(status.satellites);

  return (
    <div className="space-y-4">
      <div className="flex flex-col sm:flex-row sm:justify-between sm:items-center gap-3">
        <h2 className="text-xl sm:text-2xl font-bold text-white">
          Satellites <span className="text-gray-400 text-base sm:text-lg font-normal">({satellites.length})</span>
        </h2>
        <button
          onClick={onRefresh}
          className="px-4 py-2 bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white rounded-lg transition-all font-medium shadow-lg shadow-blue-900/50 hover:shadow-blue-900/70 text-sm sm:text-base"
        >
          ↻ Refresh
        </button>
      </div>

      {satellites.length === 0 ? (
        <div className="bg-gradient-to-br from-gray-800 to-gray-900 border border-gray-700/50 rounded-xl p-8 sm:p-12 text-center">
          <p className="text-gray-400 text-base sm:text-lg">No satellites configured</p>
        </div>
      ) : (
        <div className="grid grid-cols-1 lg:grid-cols-2 xl:grid-cols-3 gap-4">
          {satellites.map(([name, satellite]) => (
            <SatelliteCard key={name} name={name} satellite={satellite} />
          ))}
        </div>
      )}
    </div>
  );
}
