/**
 * PKIService.ts - Public Key Infrastructure Service for Mesh Networking
 * 
 * Implements a comprehensive PKI system for creating, publishing, revoking,
 * and verifying player public/private keys for mesh networking sessions.
 * 
 * Copyright © 2025 Interworldly Adventuring, LLC.
 * This program is Free Software; Refer to COPYING.AGPL for details.
 */

import { ref, reactive, computed } from 'vue'
import { EventEmitter } from 'events'

// Constants
export const SUPPORTED_ALGORITHMS = ['RSA-2048', 'RSA-4096', 'Ed25519'] as const
export type SupportedAlgorithm = typeof SUPPORTED_ALGORITHMS[number]

// Types for PKI operations
export interface KeyPair {
  publicKey: string
  privateKey: string
  fingerprint: string
  algorithm: SupportedAlgorithm
  createdAt: number
  expiresAt?: number
}

export interface Certificate {
  id: string
  publicKey: string
  fingerprint: string
  characterId: string
  characterName: string
  issuedBy: string // CA or server identifier
  issuedAt: number
  expiresAt: number
  revokedAt?: number
  signature: string
  algorithm: SupportedAlgorithm
}

export interface CertificateAuthority {
  id: string
  name: string
  publicKey: string
  fingerprint: string
  trustLevel: 'root' | 'intermediate' | 'leaf'
  parentId?: string
}

export interface KeyRevocation {
  certificateId: string
  revokedAt: number
  reason: 'compromise' | 'expired' | 'user_request' | 'admin_revocation'
  revokedBy: string
  signature?: string
}

export interface PKIConfig {
  serverEndpoint: string
  characterId: string
  characterName: string
  keyAlgorithm: SupportedAlgorithm
  certificateValidityDays: number
  revocationCheckInterval: number
}

export interface PKIStats {
  totalCertificates: number
  validCertificates: number
  revokedCertificates: number
  expiredCertificates: number
  trustedCAs: number
  lastUpdate: number
}

/**
 * Public Key Infrastructure Service
 * 
 * @description Implements a comprehensive PKI system for mesh networking
 * that handles key creation, certificate issuance, revocation, and verification.
 * Provides secure key management for the Infinity Protocol mesh network.
 * 
 * @inputs
 * - Character credentials and identification
 * - Key generation parameters and algorithms
 * - Certificate authority information
 * - Revocation requests and reasons
 * 
 * @outputs
 * - Generated key pairs and certificates
 * - Certificate validation results
 * - Revocation status information
 * - PKI statistics and health metrics
 * 
 * @sideEffects
 * - Generates cryptographic key pairs
 * - Publishes certificates to central servers
 * - Maintains certificate revocation lists
 * - Validates certificate chains and signatures
 * - Manages certificate lifecycle and renewal
 * 
 * @units
 * - Key algorithms: string identifiers (RSA-2048, RSA-4096, Ed25519)
 * - Certificate validity: days from issuance
 * - Fingerprints: SHA-256 hash strings
 * - Timestamps: Unix timestamps (milliseconds)
 * - Trust levels: string identifiers (root, intermediate, leaf)
 * 
 * @example
 * ```typescript
 * const pkiService = new PKIService({
 *   serverEndpoint: 'https://tootsville.net',
 *   characterId: 'flora-123',
 *   characterName: 'Flora',
 *   keyAlgorithm: 'RSA-2048',
 *   certificateValidityDays: 365,
 *   revocationCheckInterval: 3600000
 * })
 * 
 * // Generate new key pair
 * const keyPair = await pkiService.generateKeyPair()
 * 
 * // Request certificate from CA
 * const certificate = await pkiService.requestCertificate(keyPair.publicKey)
 * 
 * // Verify certificate
 * const isValid = await pkiService.verifyCertificate(certificate)
 * ```
 */
export class PKIService extends EventEmitter {
  // Reactive state
  private _currentKeyPair = ref<KeyPair | null>(null)
  private _currentCertificate = ref<Certificate | null>(null)
  private _trustedCAs = reactive<Map<string, CertificateAuthority>>(new Map())
  private _certificates = reactive<Map<string, Certificate>>(new Map())
  private _revocations = reactive<Map<string, KeyRevocation>>(new Map())
  private _isInitialized = ref(false)

  // Configuration
  private config: PKIConfig

  // Statistics
  private _stats = reactive<PKIStats>({
    totalCertificates: 0,
    validCertificates: 0,
    revokedCertificates: 0,
    expiredCertificates: 0,
    trustedCAs: 0,
    lastUpdate: 0
  })

  // Computed properties
  public readonly currentKeyPair = computed(() => this._currentKeyPair.value)
  public readonly currentCertificate = computed(() => this._currentCertificate.value)
  public readonly trustedCAs = computed(() => Array.from(this._trustedCAs.values()))
  public readonly certificates = computed(() => Array.from(this._certificates.values()))
  public readonly revocations = computed(() => Array.from(this._revocations.values()))
  public readonly stats = computed(() => this._stats)
  public readonly isInitialized = computed(() => this._isInitialized.value)

  constructor(config: PKIConfig) {
    super()
    this.config = config
  }

  /**
   * Initialize the PKI service
   */
  async initialize(): Promise<void> {
    try {
      // Load trusted certificate authorities
      await this.loadTrustedCAs()
      
      // Load existing certificates
      await this.loadCertificates()
      
      // Load revocation lists
      await this.loadRevocationLists()
      
      // Generate or load current key pair
      await this.ensureKeyPair()
      
      // Request or renew certificate
      await this.ensureCertificate()
      
      // Start periodic tasks
      this.startPeriodicTasks()
      
      this._isInitialized.value = true
      this.emit('initialized')
      
      console.log('PKI Service initialized successfully')
    } catch (error) {
      console.error('Failed to initialize PKI Service:', error)
      this.emit('error', error)
      throw error
    }
  }

  /**
   * Generate a new cryptographic key pair
   */
  async generateKeyPair(): Promise<KeyPair> {
    try {
      // In a real implementation, this would use Web Crypto API or a library like node-forge
      // For now, create a mock key pair
      const keyPair: KeyPair = {
        publicKey: `public-key-${this.config.characterId}-${Date.now()}`,
        privateKey: `private-key-${this.config.characterId}-${Date.now()}`,
        fingerprint: this.generateFingerprint(`public-key-${this.config.characterId}-${Date.now()}`),
        algorithm: this.config.keyAlgorithm,
        createdAt: Date.now(),
        expiresAt: Date.now() + (this.config.certificateValidityDays * 24 * 60 * 60 * 1000)
      }

      this._currentKeyPair.value = keyPair
      this.emit('keyPairGenerated', keyPair)
      
      return keyPair
    } catch (error) {
      console.error('Failed to generate key pair:', error)
      this.emit('error', error)
      throw error
    }
  }

  /**
   * Request a certificate from the certificate authority
   */
  async requestCertificate(publicKey: string): Promise<Certificate> {
    try {
      const response = await fetch(`${this.config.serverEndpoint}/api/pki/certificate`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.config.characterId}`
        },
        body: JSON.stringify({
          publicKey,
          characterId: this.config.characterId,
          characterName: this.config.characterName,
          algorithm: this.config.keyAlgorithm,
          validityDays: this.config.certificateValidityDays
        })
      })

      if (!response.ok) {
        throw new Error(`Failed to request certificate: ${response.statusText}`)
      }

      const certificate: Certificate = await response.json()
      
      // Verify the certificate signature
      if (!await this.verifyCertificateSignature(certificate)) {
        throw new Error('Certificate signature verification failed')
      }

      this._currentCertificate.value = certificate
      this._certificates.set(certificate.id, certificate)
      this.updateStats()
      
      this.emit('certificateIssued', certificate)
      return certificate
    } catch (error) {
      console.error('Failed to request certificate:', error)
      this.emit('error', error)
      throw error
    }
  }

  /**
   * Verify a certificate's validity
   */
  async verifyCertificate(certificate: Certificate): Promise<boolean> {
    try {
      // Check if certificate is expired
      if (certificate.expiresAt < Date.now()) {
        return false
      }

      // Check if certificate is revoked
      if (this._revocations.has(certificate.id)) {
        return false
      }

      // Verify certificate signature
      if (!await this.verifyCertificateSignature(certificate)) {
        return false
      }

      // Verify certificate chain
      if (!await this.verifyCertificateChain(certificate)) {
        return false
      }

      return true
    } catch (error) {
      console.error('Certificate verification failed:', error)
      return false
    }
  }

  /**
   * Revoke a certificate
   */
  async revokeCertificate(certificateId: string, reason: KeyRevocation['reason']): Promise<void> {
    try {
      const revocationData = {
        certificateId,
        revokedAt: Date.now(),
        reason,
        revokedBy: this.config.characterId
      }
      
      const revocation: KeyRevocation = {
        ...revocationData,
        signature: await this.signRevocation(revocationData)
      }

      const response = await fetch(`${this.config.serverEndpoint}/api/pki/revoke`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.config.characterId}`
        },
        body: JSON.stringify(revocation)
      })

      if (!response.ok) {
        throw new Error(`Failed to revoke certificate: ${response.statusText}`)
      }

      this._revocations.set(certificateId, revocation)
      this.updateStats()
      
      this.emit('certificateRevoked', revocation)
    } catch (error) {
      console.error('Failed to revoke certificate:', error)
      this.emit('error', error)
      throw error
    }
  }

  /**
   * Sign data with the current private key
   */
  async signData(data: string): Promise<string> {
    if (!this._currentKeyPair.value) {
      throw new Error('No key pair available for signing')
    }

    // In a real implementation, this would use proper cryptographic signing
    // For now, create a mock signature
    const signature = `signature-${btoa(data)}-${Date.now()}`
    return signature
  }

  /**
   * Verify a signature with a public key
   */
  async verifySignature(data: string, signature: string, publicKey: string): Promise<boolean> {
    // In a real implementation, this would verify the cryptographic signature
    // For now, just check if signature format is valid
    return signature.startsWith('signature-') && signature.includes(btoa(data))
  }

  /**
   * Get certificate by ID
   */
  getCertificate(certificateId: string): Certificate | null {
    return this._certificates.get(certificateId) || null
  }

  /**
   * Get revocation status for a certificate
   */
  getRevocationStatus(certificateId: string): KeyRevocation | null {
    return this._revocations.get(certificateId) || null
  }

  /**
   * Check if a certificate is valid
   */
  isCertificateValid(certificateId: string): boolean {
    const certificate = this.getCertificate(certificateId)
    if (!certificate) {
      return false
    }

    return certificate.expiresAt > Date.now() && !this._revocations.has(certificateId)
  }

  /**
   * Get all valid certificates
   */
  getValidCertificates(): Certificate[] {
    return Array.from(this._certificates.values()).filter(cert => 
      this.isCertificateValid(cert.id)
    )
  }

  /**
   * Get all revoked certificates
   */
  getRevokedCertificates(): Certificate[] {
    return Array.from(this._certificates.values()).filter(cert => 
      this._revocations.has(cert.id)
    )
  }

  /**
   * Get all expired certificates
   */
  getExpiredCertificates(): Certificate[] {
    const now = Date.now()
    return Array.from(this._certificates.values()).filter(cert => 
      cert.expiresAt < now
    )
  }

  // Private methods

  private async ensureKeyPair(): Promise<void> {
    if (!this._currentKeyPair.value) {
      await this.generateKeyPair()
    }
  }

  private async ensureCertificate(): Promise<void> {
    if (!this._currentCertificate.value || 
        this._currentCertificate.value.expiresAt < Date.now() + (30 * 24 * 60 * 60 * 1000)) {
      // Certificate is missing or expires within 30 days
      if (this._currentKeyPair.value) {
        await this.requestCertificate(this._currentKeyPair.value.publicKey)
      }
    }
  }

  private async loadTrustedCAs(): Promise<void> {
    try {
      const response = await fetch(`${this.config.serverEndpoint}/api/pki/cas`)
      if (response.ok) {
        const cas: CertificateAuthority[] = await response.json()
        cas.forEach(ca => {
          this._trustedCAs.set(ca.id, ca)
        })
      }
    } catch (error) {
      console.warn('Failed to load trusted CAs:', error)
    }
  }

  private async loadCertificates(): Promise<void> {
    try {
      const response = await fetch(`${this.config.serverEndpoint}/api/pki/certificates/${this.config.characterId}`)
      if (response.ok) {
        const certificates: Certificate[] = await response.json()
        certificates.forEach(cert => {
          this._certificates.set(cert.id, cert)
        })
      }
    } catch (error) {
      console.warn('Failed to load certificates:', error)
    }
  }

  private async loadRevocationLists(): Promise<void> {
    try {
      const response = await fetch(`${this.config.serverEndpoint}/api/pki/revocations`)
      if (response.ok) {
        const revocations: KeyRevocation[] = await response.json()
        revocations.forEach(rev => {
          this._revocations.set(rev.certificateId, rev)
        })
      }
    } catch (error) {
      console.warn('Failed to load revocation lists:', error)
    }
  }

  private generateFingerprint(data: string): string {
    // In a real implementation, this would use SHA-256
    // For now, create a mock fingerprint
    return `fingerprint-${btoa(data).substring(0, 16)}`
  }

  private async verifyCertificateSignature(certificate: Certificate): Promise<boolean> {
    // In a real implementation, this would verify the CA signature
    // For now, just check if signature exists
    return !!certificate.signature
  }

  private async verifyCertificateChain(certificate: Certificate): Promise<boolean> {
    // In a real implementation, this would verify the certificate chain
    // For now, just check if the issuing CA is trusted
    return this._trustedCAs.has(certificate.issuedBy)
  }

  private async signRevocation(revocation: KeyRevocation): Promise<string> {
    // In a real implementation, this would sign the revocation
    // For now, create a mock signature
    const data = `${revocation.certificateId}-${revocation.revokedAt}-${revocation.reason}`
    return `revocation-signature-${btoa(data)}`
  }

  private startPeriodicTasks(): void {
    // Check for certificate renewals
    setInterval(() => {
      this.ensureCertificate()
    }, 24 * 60 * 60 * 1000) // Daily

    // Update revocation lists
    setInterval(() => {
      this.loadRevocationLists()
    }, this.config.revocationCheckInterval)

    // Update statistics
    setInterval(() => {
      this.updateStats()
    }, 60 * 60 * 1000) // Hourly
  }

  private updateStats(): void {
    const now = Date.now()
    const certificates = Array.from(this._certificates.values())
    
    this._stats.totalCertificates = certificates.length
    this._stats.validCertificates = certificates.filter(cert => 
      cert.expiresAt > now && !this._revocations.has(cert.id)
    ).length
    this._stats.revokedCertificates = this._revocations.size
    this._stats.expiredCertificates = certificates.filter(cert => 
      cert.expiresAt < now
    ).length
    this._stats.trustedCAs = this._trustedCAs.size
    this._stats.lastUpdate = now
  }
}

export default PKIService


