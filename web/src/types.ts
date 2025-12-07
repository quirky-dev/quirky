export type HealthStatus = 'ok' | 'warning' | 'error';

export interface HealthResult {
  status: HealthStatus;
  message: string;
  data: any;
}

export interface SatelliteStatus {
  name: string;
  status: 'ok' | 'error';
  checks: Record<string, HealthResult>;
  error?: string;
}

export interface AggregatedStatus {
  satellites: Record<string, SatelliteStatus>;
  all_ok: boolean;
  last_update: string;
}
